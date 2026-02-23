$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);

    // Localize any times on the page
    localizeTimes()

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

// document.addEventListener('turbolinks:load', () => {
//     htmx.process(document.body);
//     _hyperscript.processNode(document.body);
// });

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
