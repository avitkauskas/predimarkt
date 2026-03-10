$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);

    initFlashToasts()
    initPasskeyAuth()

    initAutoSubmitSearchForms();

    // Localize any times on the page
    localizeTimes()

    // Initialize asset percentage calculations for market forms
    initAssetPercentageCalculations()
    initBootstrapTooltips()

    initMarketChat()
    initMarketChatTradeQuantity()
    initMarketShowTradeQuantityLinks()
    initMarketsPageOpenMarketLinks()
    initMarketPageScroll()

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
})

document.addEventListener('turbolinks:before-cache', function () {
    const container = document.getElementById('flash-toast-container')
    if (container) {
        container.remove()
    }
})

function initFlashToasts() {
    if (!window.bootstrap || typeof window.bootstrap.Toast !== 'function') return

    document.querySelectorAll('[data-auto-show-toast="true"]').forEach(toastElement => {
        if (toastElement.dataset.toastInitialized === 'true') return

        toastElement.dataset.toastInitialized = 'true'
        window.bootstrap.Toast.getOrCreateInstance(toastElement).show()
    })
}

function initBootstrapTooltips(root = document) {
    if (!window.bootstrap || typeof window.bootstrap.Tooltip !== 'function') return

    root.querySelectorAll('[data-bs-toggle="tooltip"]').forEach(tooltipElement => {
        if (tooltipElement.dataset.tooltipInitialized === 'true') return

        tooltipElement.dataset.tooltipInitialized = 'true'
        window.bootstrap.Tooltip.getOrCreateInstance(tooltipElement)
    })
}

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

function initMarketChat() {
    const messages = document.getElementById('market-chat-messages')
    if (!messages) return

    const resetToLatest = maybeResetMarketChatScroll(messages)
    if (!resetToLatest) {
        restoreMarketChatScroll(messages)
    }
    syncMarketChatMetrics(messages)

    const loadMoreLink = document.getElementById('market-chat-load-more')
    if (loadMoreLink && !loadMoreLink.marketChatInitialized) {
        loadMoreLink.marketChatInitialized = true
        loadMoreLink.addEventListener('click', function (event) {
            event.preventDefault()
            navigateMarketChat(messages, loadMoreLink.dataset.chatLoadMoreUrl || loadMoreLink.href)
        })
    }

    if (messages.marketChatInitialized) return

    messages.marketChatInitialized = true

    messages.addEventListener('scroll', () => {
        messages.marketChatScrollTouched = true
        syncMarketChatMetrics(messages)
        maybeLoadOlderMarketChat(messages)
    })

    if (messages.marketChatObserver) {
        messages.marketChatObserver.disconnect()
    }

    messages.marketChatObserver = new MutationObserver(() => {
        const previousScrollHeight = messages.marketChatLastScrollHeight || messages.scrollHeight
        const previousScrollTop = messages.marketChatLastScrollTop || 0
        const wasNearTop = previousScrollTop <= 32
        const nextScrollHeight = messages.scrollHeight

        if (!wasNearTop && nextScrollHeight > previousScrollHeight) {
            messages.scrollTop = previousScrollTop + (nextScrollHeight - previousScrollHeight)
        }

        syncMarketChatMetrics(messages)
    })

    messages.marketChatObserver.observe(messages, { childList: true, subtree: true })
}

function initMarketPageScroll() {
    const chatCard = document.getElementById('market-chat-card')
    if (!chatCard) return

    if (window.location.pathname !== '/ShowMarket') {
        window.marketPageShouldStartAtTop = false
        return
    }

    if (!window.marketPageShouldStartAtTop) return

    window.marketPageShouldStartAtTop = false

    requestAnimationFrame(() => {
        window.scrollTo(0, 0)
    })
}

function initMarketsPageOpenMarketLinks() {
    document.querySelectorAll('a[data-start-market-page-at-top="true"]').forEach(link => {
        if (link.marketPageStartTopInitialized) return
        link.marketPageStartTopInitialized = true

        link.addEventListener('click', function () {
            window.marketPageShouldStartAtTop = true
        })
    })
}

window.visitGetFormWithTurbolinks = function (form) {
    const action = new URL(form.getAttribute('action') || window.location.pathname, window.location.origin)
    const params = new URLSearchParams()

    for (const [key, rawValue] of new FormData(form).entries()) {
        if (typeof rawValue !== 'string') continue

        const value = rawValue.trim()
        if (value === '') continue

        params.append(key, value)
    }

    const nextUrl = params.toString()
        ? `${action.pathname}?${params.toString()}`
        : action.pathname

    if (window.Turbolinks && typeof window.Turbolinks.visit === 'function') {
        window.Turbolinks.visit(nextUrl)
        return
    }

    window.location.assign(nextUrl)
}

function initMarketChatTradeQuantity() {
    const chatForm = document.getElementById('market-chat-form')
    const chatTradeQuantityInput = document.getElementById('market-chat-trade-quantity')
    if (!chatForm || !chatTradeQuantityInput || chatForm.marketTradeQuantityInitialized) return

    chatForm.marketTradeQuantityInitialized = true
    chatForm.addEventListener('submit', function () {
        const activeTradeQuantityInput = document.querySelector(
            '[id^="buy-form-"]:not(.d-none) input[name="quantity"], [id^="sell-form-"]:not(.d-none) input[name="quantity"]'
        )

        chatTradeQuantityInput.value = activeTradeQuantityInput ? activeTradeQuantityInput.value : ''
    })
}

function initMarketShowTradeQuantityLinks() {
    if (window.location.pathname !== '/ShowMarket') return

    const syncLinks = () => {
        const tradeQuantity = currentMarketTradeQuantity()

        document.querySelectorAll('a[href]').forEach(link => {
            const url = new URL(link.href, window.location.origin)
            if (url.origin !== window.location.origin || url.pathname !== '/ShowMarket') return

            if (tradeQuantity === '') {
                url.searchParams.delete('tradeQuantity')
            } else {
                url.searchParams.set('tradeQuantity', tradeQuantity)
            }

            link.href = url.toString()
        })
    }

    syncLinks()

    document.querySelectorAll('[id^="buy-form-"] input[name="quantity"], [id^="sell-form-"] input[name="quantity"]').forEach(input => {
        if (input.marketTradeQuantityLinksInitialized) return
        input.marketTradeQuantityLinksInitialized = true
        input.addEventListener('input', syncLinks)
        input.addEventListener('change', syncLinks)
    })
}

function currentMarketTradeQuantity() {
    const activeTradeQuantityInput = document.querySelector(
        '[id^="buy-form-"]:not(.d-none) input[name="quantity"], [id^="sell-form-"]:not(.d-none) input[name="quantity"]'
    )
    if (activeTradeQuantityInput) return activeTradeQuantityInput.value.trim()

    const currentUrl = new URL(window.location.href)
    return (currentUrl.searchParams.get('tradeQuantity') || '').trim()
}

function syncMarketChatMetrics(messages) {
    messages.marketChatLastScrollTop = messages.scrollTop
    messages.marketChatLastScrollHeight = messages.scrollHeight
}

function maybeLoadOlderMarketChat(messages) {
    const nextUrl = messages.dataset.nextUrl
    if (!nextUrl || messages.dataset.loading === 'true' || !messages.marketChatScrollTouched) return

    const nearBottom = messages.scrollTop + messages.clientHeight >= messages.scrollHeight - 32
    if (!nearBottom) return

    navigateMarketChat(messages, nextUrl)
}

function navigateMarketChat(messages, nextUrl) {
    if (!nextUrl) return

    messages.dataset.loading = 'true'
    rememberMarketChatScroll(messages)

    if (window.Turbolinks && typeof window.Turbolinks.visit === 'function') {
        window.Turbolinks.visit(nextUrl)
        return
    }

    window.location.assign(nextUrl)
}

function rememberMarketChatScroll(messages) {
    const scrollKey = messages.dataset.scrollKey
    if (!scrollKey) return
    sessionStorage.setItem(scrollKey, String(messages.scrollTop))
}

function maybeResetMarketChatScroll(messages) {
    const scrollKey = messages.dataset.scrollKey
    const composerRevision = messages.dataset.composerRev
    if (!scrollKey || !composerRevision) return false

    const revisionKey = `${scrollKey}-composer-rev`
    const lastComposerRevision = sessionStorage.getItem(revisionKey)

    if (lastComposerRevision === composerRevision) return false

    sessionStorage.setItem(revisionKey, composerRevision)
    sessionStorage.removeItem(scrollKey)
    messages.scrollTop = 0
    return true
}

function restoreMarketChatScroll(messages) {
    const scrollKey = messages.dataset.scrollKey
    if (!scrollKey) return

    const savedScrollTop = sessionStorage.getItem(scrollKey)
    if (savedScrollTop === null) return

    messages.scrollTop = Number(savedScrollTop)
    sessionStorage.removeItem(scrollKey)
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
    initBootstrapTooltips(e.target)

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

function initPasskeyAuth() {
    document.querySelectorAll('.js-passkey-login').forEach(container => {
        if (container.dataset.passkeyInitialized === 'true') return
        container.dataset.passkeyInitialized = 'true'

        const button = container.querySelector('.js-passkey-login-button')
        if (!button) return

        button.addEventListener('click', async function () {
            await runPasskeyLogin(container, button)
        })
    })

    document.querySelectorAll('.js-passkey-register').forEach(container => {
        if (container.dataset.passkeyInitialized === 'true') return
        container.dataset.passkeyInitialized = 'true'

        const button = container.querySelector('.js-passkey-register-button')
        if (!button) return

        button.addEventListener('click', async function () {
            await runPasskeyRegistration(container, button)
        })
    })
}

async function runPasskeyLogin(container, button) {
    await withPasskeyButton(button, async () => {
        setPasskeyStatus(container, 'info', 'Waiting for your passkey…')

        const beginResponse = await postJson(container.dataset.beginUrl, {})
        const credential = await navigator.credentials.get({
            publicKey: authenticationOptionsToNative(beginResponse)
        })

        if (!credential) throw new Error('No passkey was selected.')

        const finishResponse = await postJson(
            container.dataset.finishUrl,
            serializeAuthenticationCredential(credential)
        )

        setPasskeyStatus(container, 'success', 'Logged in successfully.')
        redirectAfterPasskeySuccess(container, finishResponse)
    })
}

async function runPasskeyRegistration(container, button) {
    await withPasskeyButton(button, async () => {
        const nickname = nicknameForContainer(container)
        if (container.dataset.nicknameInputId && nickname === '') {
            throw new Error('Please choose a nickname.')
        }

        setPasskeyStatus(container, 'info', 'Waiting for your passkey…')

        const beginResponse = await postJson(container.dataset.beginUrl, nickname ? { nickname } : {})
        const credential = await navigator.credentials.create({
            publicKey: registrationOptionsToNative(beginResponse)
        })

        if (!credential) throw new Error('Passkey registration was cancelled.')

        const finishResponse = await postJson(
            container.dataset.finishUrl,
            serializeRegistrationCredential(credential)
        )

        setPasskeyStatus(container, 'success', finishResponse.message || 'Passkey saved successfully.')
        redirectAfterPasskeySuccess(container, finishResponse)
    })
}

async function withPasskeyButton(button, callback) {
    if (!window.PublicKeyCredential || !navigator.credentials) {
        setPasskeyStatus(button.closest('.js-passkey-login, .js-passkey-register'), 'danger', 'Passkeys are not supported in this browser.')
        return
    }

    const originalHtml = button.innerHTML
    button.disabled = true
    button.innerHTML = '<span class="spinner-border spinner-border-sm me-2" role="status" aria-hidden="true"></span>Please wait'

    try {
        await callback()
    } catch (error) {
        setPasskeyStatus(button.closest('.js-passkey-login, .js-passkey-register'), 'danger', error.message || 'Passkey request failed.')
    } finally {
        button.disabled = false
        button.innerHTML = originalHtml
    }
}

async function postJson(url, payload) {
    const response = await fetch(url, {
        method: 'POST',
        credentials: 'same-origin',
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        body: JSON.stringify(payload)
    })

    const json = await response.json().catch(() => ({}))
    if (!response.ok) {
        throw new Error(json.error || 'Passkey request failed.')
    }

    return json
}

function nicknameForContainer(container) {
    const inputId = container.dataset.nicknameInputId
    if (!inputId) return ''

    const input = document.getElementById(inputId)
    return input ? input.value.trim() : ''
}

function setPasskeyStatus(container, tone, message) {
    if (!container) return

    const targetId = container.dataset.statusId
    if (!targetId) return

    const element = document.getElementById(targetId)
    if (!element) return

    element.className = `alert alert-${tone} mb-3`
    element.textContent = message
}

function redirectAfterPasskeySuccess(container, response) {
    const redirectTo = response.redirectTo || container.dataset.successRedirect
    if (!redirectTo) return

    if (window.Turbolinks && typeof window.Turbolinks.visit === 'function') {
        window.Turbolinks.visit(redirectTo)
        return
    }

    window.location.assign(redirectTo)
}

function registrationOptionsToNative(options) {
    if (!options.user?.id) {
        throw new Error('Invalid registration options: missing user information')
    }
    return {
        ...options,
        challenge: base64UrlToBuffer(options.challenge),
        user: {
            ...options.user,
            id: base64UrlToBuffer(options.user.id)
        },
        excludeCredentials: (options.excludeCredentials || []).map(descriptor => ({
            ...descriptor,
            id: base64UrlToBuffer(descriptor.id)
        }))
    }
}

function authenticationOptionsToNative(options) {
    return {
        ...options,
        challenge: base64UrlToBuffer(options.challenge),
        allowCredentials: (options.allowCredentials || []).map(descriptor => ({
            ...descriptor,
            id: base64UrlToBuffer(descriptor.id)
        }))
    }
}

function serializeRegistrationCredential(credential) {
    return {
        rawId: bufferToBase64Url(credential.rawId),
        response: {
            clientDataJSON: bufferToBase64Url(credential.response.clientDataJSON),
            attestationObject: bufferToBase64Url(credential.response.attestationObject),
            transports: typeof credential.response.getTransports === 'function'
                ? credential.response.getTransports()
                : []
        },
        clientExtensionResults: credential.getClientExtensionResults()
    }
}

function serializeAuthenticationCredential(credential) {
    return {
        rawId: bufferToBase64Url(credential.rawId),
        response: {
            clientDataJSON: bufferToBase64Url(credential.response.clientDataJSON),
            authenticatorData: bufferToBase64Url(credential.response.authenticatorData),
            signature: bufferToBase64Url(credential.response.signature),
            userHandle: credential.response.userHandle
                ? bufferToBase64Url(credential.response.userHandle)
                : null
        },
        clientExtensionResults: credential.getClientExtensionResults()
    }
}

function base64UrlToBuffer(value) {
    const normalized = value.replace(/-/g, '+').replace(/_/g, '/')
    const padded = normalized + '='.repeat((4 - normalized.length % 4) % 4)
    const binary = window.atob(padded)
    const bytes = new Uint8Array(binary.length)

    for (let index = 0; index < binary.length; index += 1) {
        bytes[index] = binary.charCodeAt(index)
    }

    return bytes.buffer
}

function bufferToBase64Url(buffer) {
    const bytes = new Uint8Array(buffer)
    let binary = ''

    bytes.forEach(byte => {
        binary += String.fromCharCode(byte)
    })

    return window.btoa(binary)
        .replace(/\+/g, '-')
        .replace(/\//g, '_')
        .replace(/=+$/g, '')
}
