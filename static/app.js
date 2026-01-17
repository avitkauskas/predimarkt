$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    htmx.process(document.body);
});

// document.addEventListener('turbolinks:load', () => {
//     htmx.process(document.body);
//     _hyperscript.processNode(document.body);
// });