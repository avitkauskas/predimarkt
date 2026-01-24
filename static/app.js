$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);
});

// document.addEventListener('turbolinks:load', () => {
//     htmx.process(document.body);
//     _hyperscript.processNode(document.body);
// });

$(document).on('ready turbolinks:load', function () {
    $('.my-flatpickr').each(function () {
        const $el = $(this);

        flatpickr(this, {
            time_24hr: true,
            dateFormat: 'Z',
            altInput: true,
            altFormat: $el.data('alt-format') || 'Y-m-d H:i',
            // enableTime: $el.data('enable-time') === true,
            enableTime: true,
            monthSelectorType: 'static',
            allowInput: true,
        });
    });
});
