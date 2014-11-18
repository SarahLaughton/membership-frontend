/*global ga */
define(['string_score', 'bean', '$'], function (string_score, bean, $) {

    var filterInput  = document.getElementById('js-filter'),
        filterParent = document.getElementById('js-filter-container'),
        filterField  = filterInput.getAttribute('data-filter-field'),
        throttle     = 300, // how many milliseconds should we wait for typing to pause?
        currentTimeout;

    // track what people filter on
    var trackSearch = function (category, action, label) {
        ga('send', 'event', category, action, label);
    };

    // create an index mapping any filter "key"
    // (eg. title, price) to DOM elements
    var index = $('.js-filter-item').map(function (item) {
        var filters = {};
        $('[data-filter-key]', item).each(function (f) {
            var elm = $(f);
            filters[elm.data('filter-key')] = elm.text();
        });
        return {
            elm: item,
            filters: filters
        };
    });

    // filter the list based on the index
    var filterList = function (e) {
        e.preventDefault();

        if (currentTimeout) { window.clearTimeout(currentTimeout); }

        // start search when the user pauses typing
        currentTimeout = window.setTimeout(function () {

            var value = filterInput.value;
            var elmsToShow = [],
                elmsToHide = [];

            if (value) {
                trackSearch('Event filter', 'Masterclasses', value);
            }

            index.forEach(function (item) {
                // use simple substring matching for now...
                var isFound = item.filters[filterField].toLowerCase().search(value);
                if (isFound !== -1) {
                    elmsToShow.push(item.elm);
                } else {
                    elmsToHide.push(item.elm);
                }
            });

            // remove the non-matching elements from the DOM
            $(elmsToHide).detach();

            // show matching elements
            // (which may have been hidden in previous searches)
            $(elmsToShow).appendTo(filterParent);

            // if no results, we show a message
            if (!elmsToShow.length) {
                $(filterParent).addClass('events-list--empty');
            } else {
                $(filterParent).removeClass('events-list--empty');
            }

        }, throttle);

    };

    // bind to typing in the search box
    bean.on(filterInput, 'keyup', filterList);

});