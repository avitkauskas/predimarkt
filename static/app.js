$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);

    // Localize any times on the page
    localizeTimes()

    // Initialize asset percentage calculations for market forms
    initAssetPercentageCalculations()

    // Initialize info blocks for any pre-opened trade forms (only on market show pages)
    const buyForms = document.querySelectorAll('[id^="buy-form-"]:not(.d-none)');
    const sellForms = document.querySelectorAll('[id^="sell-form-"]:not(.d-none)');

    if (buyForms.length > 0 || sellForms.length > 0) {
        buyForms.forEach(form => {
            const input = form.querySelector('input[type="number"]');
            if (input && input.value && Number(input.value) > 0) {
                updateBuyInfo(input);
            }
        });

        sellForms.forEach(form => {
            const input = form.querySelector('input[type="number"]');
            if (input && input.value && Number(input.value) > 0) {
                updateSellInfo(input);
            }
        });
    }
});

// Preserve search input value and focus after HTMX swaps
(function () {
    // Helper to get storage key based on current page
    function getStorageKey(baseKey) {
        var path = window.location.pathname;
        var pageKey;
        if (path.includes('/DashboardPositions')) {
            pageKey = 'positions';
        } else if (path.includes('/DashboardTransactions')) {
            pageKey = 'transactions';
        } else {
            pageKey = 'markets';
        }
        return baseKey + '_' + pageKey;
    }

    document.addEventListener('input', function (evt) {
        if (evt.target && evt.target.matches && evt.target.matches('input[type="search"]')) {
            const value = evt.target.value;
            const searchValueKey = getStorageKey('searchValue');
            const searchCursorKey = getStorageKey('searchCursor');
            if (value) {
                localStorage.setItem(searchValueKey, value);
                localStorage.setItem(searchCursorKey, evt.target.selectionStart.toString());
            } else {
                localStorage.removeItem(searchValueKey);
                localStorage.removeItem(searchCursorKey);
            }
        }
    });

    document.addEventListener('htmx:after:swap', function () {
        var searchValueKey = getStorageKey('searchValue');
        var searchCursorKey = getStorageKey('searchCursor');
        var savedSearchValue = localStorage.getItem(searchValueKey) || '';
        var savedCursorPos = parseInt(localStorage.getItem(searchCursorKey) || '0', 10);
        var searchInput = document.querySelector('#search-form-container input[type="search"]');

        if (searchInput) {
            searchInput.value = savedSearchValue;
            searchInput.focus();
            searchInput.setSelectionRange(savedCursorPos, savedCursorPos);
        }
    });

    // Clear stored search data on form submission
    document.addEventListener('submit', function (evt) {
        if (evt.target && evt.target.action) {
            var action = evt.target.action;
            if (action.includes('/Markets')) {
                localStorage.removeItem('searchValue_markets');
                localStorage.removeItem('searchCursor_markets');
            }
            else if (action.includes('/DashboardPositions')) {
                localStorage.removeItem('searchValue_positions');
                localStorage.removeItem('searchCursor_positions');
            }
            else if (action.includes('/DashboardTransactions')) {
                localStorage.removeItem('searchValue_transactions');
                localStorage.removeItem('searchCursor_transactions');
            }
        }
    });

    // Clear stored search data on page unload
    window.addEventListener('beforeunload', function () {
        // Clear all keys on unload to avoid stale data
        localStorage.removeItem('searchValue_markets');
        localStorage.removeItem('searchCursor_markets');
        localStorage.removeItem('searchValue_positions');
        localStorage.removeItem('searchCursor_positions');
        localStorage.removeItem('searchValue_transactions');
        localStorage.removeItem('searchCursor_transactions');
    });
})();

// Remove empty search parameter from HTMX requests to keep URLs clean
document.addEventListener('htmx:config:request', function (evt) {
    if (evt.detail.ctx && evt.detail.ctx.request && evt.detail.ctx.request.body) {
        const body = evt.detail.ctx.request.body;
        const searchValue = body.get('search');
        if (searchValue === '' || searchValue === null) {
            body.delete('search');
        }
    }
});

window.toggleAssetForm = function (assetId, type) {
    const buyForm = document.getElementById(`buy-form-${assetId}`);
    const sellForm = document.getElementById(`sell-form-${assetId}`);

    if (type === 'buy') {
        sellForm.classList.add('d-none');
        const wasHidden = buyForm.classList.contains('d-none');
        buyForm.classList.toggle('d-none');

        // Trigger info update if form was just opened and has a value
        if (wasHidden && !buyForm.classList.contains('d-none')) {
            const input = buyForm.querySelector('input[type="number"]');
            if (input && input.value) {
                updateBuyInfo(input);
            }
        }
    } else {
        buyForm.classList.add('d-none');
        const wasHidden = sellForm.classList.contains('d-none');
        sellForm.classList.toggle('d-none');

        // Trigger info update if form was just opened and has a value
        if (wasHidden && !sellForm.classList.contains('d-none')) {
            const input = sellForm.querySelector('input[type="number"]');
            if (input && input.value) {
                updateSellInfo(input);
            }
        }
    }
}

window.lmsrCore = function (a, z, sign) {
    const la = Math.log(a)
    const lb = Math.log(1 - a)

    const t1 = la
    const t2 = lb + sign * z

    const m = Math.max(t1, t2)

    const logD = m + Math.log(Math.exp(t1 - m) + Math.exp(t2 - m))

    const pNew = Math.exp(la - logD)

    return { logD, pNew }
}

window.lmsrPreview = function ({ x, a, beta, sign }) {
    const z = x / beta
    const { logD, pNew } = lmsrCore(a, z, sign)

    const money =
        sign < 0
            ? x + beta * logD   // BUY: invested
            : x - beta * logD   // SELL: received

    return {
        money,
        pNew,
        net: x - money
    }
}

window.updateBuyInfo = function (input) {
    const x = Number(input.value || 0)
    const containerId = input.dataset.infoId
    const container = document.getElementById(containerId)
    if (!container) return

    if (x <= 0) {
        container.classList.add('d-none')
        return
    }
    container.classList.remove('d-none')

    const { money, pNew, net } = lmsrPreview({
        x,
        a: Number(input.dataset.a),
        beta: Number(input.dataset.beta),
        sign: -1
    })

    container.innerHTML = `
        <div class="trade-info-grid">
            <div class="info-item">
                <span class="info-label">Invest</span>
                <span class="info-value">€${money.toFixed(2)}</span>
            </div>
            <div class="info-item">
                <span class="info-label">Return</span>
                <span class="info-value">${(net / money * 100).toFixed(1)}%</span>
            </div>
            <div class="info-item">
                <span class="info-label">Gain</span>
                <span class="info-value text-success">€${net.toFixed(2)}</span>
            </div>
            <div class="info-item">
                <span class="info-label">Probability</span>
                <span class="info-value">
                    <span class="info-transition">↑</span>
                    ${(pNew * 100).toFixed(2)}%
                </span>
            </div>
        </div>
    `
}

window.updateSellInfo = function (input) {
    const x = Number(input.value || 0)
    const containerId = input.dataset.infoId
    const container = document.getElementById(containerId)
    if (!container) return

    if (x <= 0) {
        container.classList.add('d-none')
        return
    }
    container.classList.remove('d-none')

    const { money, pNew, net } = lmsrPreview({
        x,
        a: Number(input.dataset.a),
        beta: Number(input.dataset.beta),
        sign: +1
    })

    container.innerHTML = `
        <div class="trade-info-grid">
            <div class="info-item">
                <span class="info-label">Receive</span>
                <span class="info-value">€${money.toFixed(2)}</span>
            </div>
            <div class="info-item">
                <span class="info-label">Return</span>
                <span class="info-value">${(money / net * 100).toFixed(1)}%</span>
            </div>
            <div class="info-item">
                <span class="info-label">Risk</span>
                <span class="info-value text-danger">€${net.toFixed(2)}</span>
            </div>
            <div class="info-item">
                <span class="info-label">Probability</span>
                <span class="info-value">
                    <span class="info-transition">↓</span>
                    ${(pNew * 100).toFixed(2)}%
                </span>
            </div>
        </div>
    `
}

function formatLocalISO(date) {
    const pad = (n) => String(n).padStart(2, '0');

    const year = date.getFullYear();
    const month = pad(date.getMonth() + 1);
    const day = pad(date.getDate());

    const hours = pad(date.getHours());
    const minutes = pad(date.getMinutes());
    const seconds = pad(date.getSeconds());

    return `${year}-${month}-${day} ${hours}:${minutes}:${seconds}`;
}

function localizeTimes(root = document) {
    root.querySelectorAll(".local-time").forEach(el => {
        if (el.dataset.localized) return;

        let timeStr = el.dataset.time;
        timeStr = timeStr.replace(" UTC", "Z").replace(" ", "T");

        const date = new Date(timeStr);
        if (isNaN(date)) return; // safety guard

        el.textContent = formatLocalISO(date);
        el.dataset.localized = "true";
    });
}

// LMSR Probability Calculations for Market Forms
window.calculateLMSRProbabilities = function (quantities, beta) {
    if (beta === 0 || quantities.length === 0) {
        return quantities.map(() => 0);
    }

    const scaled = quantities.map(q => q / beta);
    const max = Math.max(...scaled);
    const exps = scaled.map(s => Math.exp(s - max));
    const sum = exps.reduce((a, b) => a + b, 0);

    return exps.map(e => e / sum);
};

window.updateAssetPercentages = function () {
    const assetsList = document.getElementById('assets-list');
    if (!assetsList) return;

    const beta = parseFloat(assetsList.dataset.beta) || 300;
    const rows = assetsList.querySelectorAll('.asset-row');

    const quantities = [];
    const percentageElements = [];

    rows.forEach(row => {
        const qtyInput = row.querySelector('.asset-quantity');
        const pctDisplay = row.querySelector('.asset-percentage');

        if (qtyInput && pctDisplay) {
            const qty = parseFloat(qtyInput.value) || 0;
            quantities.push(qty);
            percentageElements.push(pctDisplay);
        }
    });

    const probabilities = calculateLMSRProbabilities(quantities, beta);

    percentageElements.forEach((el, index) => {
        const pct = Math.round(probabilities[index] * 100);
        el.textContent = pct + '%';
    });
};

window.deleteAssetRow = function (element) {
    const rows = document.querySelectorAll('.asset-row');
    if (rows.length <= 2) {
        alert('Market must have at least 2 assets.');
        return;
    }

    const row = element.closest('.asset-row');
    if (row) {
        row.remove();
        updateAssetPercentages();
    }
};

function initAssetPercentageCalculations() {
    const assetsList = document.getElementById('assets-list');
    if (!assetsList) return;

    // Remove existing listener to avoid duplicates
    assetsList.removeEventListener('input', handleAssetQuantityInput);
    // Listen for input events on quantity fields (use event delegation)
    assetsList.addEventListener('input', handleAssetQuantityInput);

    // Set up mutation observer to watch for new asset rows being added
    if (assetsList._mutationObserver) {
        assetsList._mutationObserver.disconnect();
    }
    assetsList._mutationObserver = new MutationObserver(function (mutations) {
        // Recalculate when child nodes are added or removed
        updateAssetPercentages();
    });
    assetsList._mutationObserver.observe(assetsList, { childList: true });

    // Initial calculation
    updateAssetPercentages();
}

function handleAssetQuantityInput(e) {
    if (e.target.classList.contains('asset-quantity')) {
        updateAssetPercentages();
    }
}

// HTMX after swap handler for adding new assets
document.addEventListener('htmx:after:swap', function (e) {
    // Localize times in the swapped content
    localizeTimes(e.target)

    // Check if this swap is targeting the assets-list (where new assets are added via beforeend)
    // e.target is the element that received the swapped content
    if (e.target.id === 'assets-list') {
        // Use a small delay to ensure the new DOM elements are fully inserted
        setTimeout(updateAssetPercentages, 50)
        return
    }

    // Also check if the swapped fragment itself contains an asset row
    // (handles cases where the response is a single asset row)
    if (e.detail && e.detail.fragment) {
        if (e.detail.fragment.querySelector && e.detail.fragment.querySelector('.asset-row')) {
            setTimeout(updateAssetPercentages, 50)
            return
        }
    }
})

// Also handle HTMX after settle (when content is fully settled in DOM)
document.addEventListener('htmx:after:settle', function (e) {
    if (e.target.id === 'assets-list' || e.target.querySelector('.asset-row')) {
        setTimeout(updateAssetPercentages, 50)
    }
})
