(() => {
    const getStoredTheme = () => localStorage.getItem('theme')

    const getPreferredTheme = () => {
        const stored = getStoredTheme()
        if (stored) return stored
        return window.matchMedia('(prefers-color-scheme: dark)').matches
            ? 'dark'
            : 'light'
    }

    const updateIcons = theme => {
        document.querySelectorAll('[data-theme-icon]').forEach(el => {
            el.textContent = theme === 'dark' ? '☀︎' : '☾'
        })
    }

    const applyTheme = theme => {
        document.documentElement.setAttribute('data-bs-theme', theme)
    }

    const syncUI = () => {
        const theme =
            document.documentElement.getAttribute('data-bs-theme')
            || getPreferredTheme()

        applyTheme(theme)
        updateIcons(theme)
    }

    applyTheme(getPreferredTheme())


    document.addEventListener('turbolinks:load', syncUI)
    document.addEventListener('htmx:after:swap', syncUI)
    document.addEventListener('turbolinks:before-render', (event) => {
        const theme = document.documentElement.getAttribute('data-bs-theme') || getPreferredTheme()
        const newBody = event.data?.newBody || event.detail?.newBody
        if (!newBody) return
        const icons = newBody.querySelectorAll('[data-theme-icon]')
        icons.forEach(el => {
            el.textContent = theme === 'dark' ? '☀︎' : '☾'
        })
    })

    window.toggleTheme = () => {
        const current = document.documentElement.getAttribute('data-bs-theme')
        const next = current === 'dark' ? 'light' : 'dark'
        localStorage.setItem('theme', next)
        applyTheme(next)
        updateIcons(next)

        if (window.Turbolinks) {
            window.Turbolinks.clearCache()
        }
    }
})()
