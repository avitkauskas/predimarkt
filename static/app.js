$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);
});

// document.addEventListener('turbolinks:load', () => {
//     htmx.process(document.body);
//     _hyperscript.processNode(document.body);
// });

$(document).on('ready turbolinks:load', function () {
    document.querySelectorAll(".js-flatpickr").forEach(el => {
        flatpickr(el, {
            time_24hr: true,
            dateFormat: 'Z',
            altInput: true,
            altFormat: el.dataset.altFormat || 'Y-m-d H:i',
            enableTime: el.dataset.enableTime === 'true',
            monthSelectorType: 'static',
            allowInput: true,
        });
    });
});
