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