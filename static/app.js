$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);
});

// document.addEventListener('turbolinks:load', () => {
//     htmx.process(document.body);
//     _hyperscript.processNode(document.body);
// });

window.initDatePicker = function () {
    if (!('flatpickr' in window)) {
        return;
    }

    document.querySelectorAll("input[type='date']").forEach(el => {
        flatpickr(el, {
            ...(el.dataset.altFormat ? {} : { altFormat: 'd.m.y' }),
            ...(el.dataset.altInput ? {} : { altInput: true }),
        });
    });

    document.querySelectorAll("input[type='datetime-local']").forEach(el => {
        flatpickr(el, {
            ...(el.dataset.enableTime ? {} : { enableTime: true }),
            ...(el.dataset.time_24hr ? {} : { time_24hr: true }),
            ...(el.dataset.dateFormat ? {} : { dateFormat: 'Z' }),
            ...(el.dataset.altFormat ? {} : { altFormat: 'd.m.y, H:i' }),
            ...(el.dataset.altInput ? {} : { altInput: true }),
        });
    });
}

window.toggleAssetForm = function (assetId, type) {
    const buyForm = document.getElementById(`buy-form-${assetId}`);
    const sellForm = document.getElementById(`sell-form-${assetId}`);

    if (type === 'buy') {
        sellForm.classList.add('d-none');
        buyForm.classList.toggle('d-none');
    } else {
        buyForm.classList.add('d-none');
        sellForm.classList.toggle('d-none');
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
    if (x <= 0) return

    const { money, pNew, net } = lmsrPreview({
        x,
        a: Number(input.dataset.a),
        beta: Number(input.dataset.beta),
        sign: -1
    })

    document.getElementById(input.dataset.infoId).innerText =
        `Invest €${money.toFixed(2)} ` +
        `expecting to gain €${net.toFixed(2)} ` +
        `(return of ${(net / money * 100).toFixed(1)}%) ` +
        `changing the probability to ${(pNew * 100).toFixed(2)}%`
}

window.updateSellInfo = function (input) {
    const x = Number(input.value || 0)
    if (x <= 0) return

    const { money, pNew, net } = lmsrPreview({
        x,
        a: Number(input.dataset.a),
        beta: Number(input.dataset.beta),
        sign: +1
    })

    document.getElementById(input.dataset.infoId).innerText =
        `Receive €${money.toFixed(2)} ` +
        `risking to lose €${net.toFixed(2)} ` +
        `(return of ${(money / net * 100).toFixed(1)}%) ` +
        `changing the probability to ${(pNew * 100).toFixed(2)}%`
}