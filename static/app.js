$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);

    initAutoSubmitSearchForms();

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

function initAutoSubmitSearchForms() {
    const forms = document.querySelectorAll('form[data-auto-submit-delay]');

    forms.forEach(form => {
        if (form.autoSubmitInitialized) return;
        form.autoSubmitInitialized = true;

        const delay = Number(form.dataset.autoSubmitDelay || '300');
        const searchInput = form.querySelector('input[type="search"]');
        if (!searchInput) return;

        const scheduleSubmit = () => {
            if (form.autoSubmitTimeout) {
                clearTimeout(form.autoSubmitTimeout);
            }

            form.autoSubmitTimeout = setTimeout(() => {
                window.submitForm(form);
            }, delay);
        };

        const clearScheduledSubmit = () => {
            if (form.autoSubmitTimeout) {
                clearTimeout(form.autoSubmitTimeout);
                form.autoSubmitTimeout = null;
            }
        };

        searchInput.addEventListener('input', scheduleSubmit);
        searchInput.addEventListener('search', scheduleSubmit);
        form.addEventListener('submit', clearScheduledSubmit);
    });
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

function formatTradeNumber(value, digits) {
    return Number.isFinite(value) ? value.toFixed(digits) : '--'
}

function formatTradePercent(value, digits) {
    return Number.isFinite(value) ? `${value.toFixed(digits)}%` : '--'
}

function setTradeInfoValue(container, fieldName, value) {
    const field = container.querySelector(`[data-trade-field="${fieldName}"]`)
    if (field) field.textContent = value
}

function updateTradeInfo(input, { sign, returnPercent }) {
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
        sign
    })

    setTradeInfoValue(container, 'money', formatTradeNumber(money, 2))
    setTradeInfoValue(container, 'return', formatTradePercent(returnPercent({ money, net }), 1))
    setTradeInfoValue(container, 'net', formatTradeNumber(net, 2))
    setTradeInfoValue(container, 'probability', formatTradePercent(pNew * 100, 2))
}

window.updateBuyInfo = function (input) {
    updateTradeInfo(input, {
        sign: -1,
        returnPercent: ({ money, net }) => net / money * 100
    })
}

window.updateSellInfo = function (input) {
    updateTradeInfo(input, {
        sign: +1,
        returnPercent: ({ money, net }) => money / net * 100
    })
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
