define(['src/utils/decodeBase64'], function (decodeBase64) {

    /*
     Cookie functions originally from http://www.quirksmode.org/js/cookies.html
     These are secure cookies and the value is JSON.stringify on set and JSON.parse on get
     */
    var doc = document;

    function setCookie(name, value, days, isUnSecure) {
        var date;
        var expires;
        // used for testing purposes, cookies are secure by default
        var secureCookieString = isUnSecure ? '' : '; secure';

        if (days) {
            date = new Date();
            date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
            expires = '; expires=' + date.toGMTString();
        } else {
            expires = '';
        }

        if (typeof value === 'object') {
            value = JSON.stringify(value);
        }

        doc.cookie = [name, '=', value, expires, '; path=/', secureCookieString ].join('');
    }

    function getCookie(name) {
        var nameEQ = name + '=';
        var ca = document.cookie.split(';');
        for (var i = 0; i < ca.length; i++) {
            var c = ca[i];
            while (c.charAt(0) === ' ') { c = c.substring(1, c.length); }
            if (c.indexOf(nameEQ) === 0) { return c.substring(nameEQ.length, c.length); }
        }
        return null;
    }

    function getDecodedCookie(name) {
        var cookieData = getCookie(name);
        return decodeCookie(cookieData);
    }

    function removeCookie(name) {
        setCookie(name, '', -1);
    }

    function decodeCookie(cookieData) {
        return (cookieData) ? JSON.parse(decodeBase64(cookieData.split('.')[0])) : null;
    }

    return {
        setCookie: setCookie,
        getCookie: getCookie,
        removeCookie: removeCookie,
        decodeCookie: decodeCookie,
        getDecodedCookie: getDecodedCookie
    };

});
