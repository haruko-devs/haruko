(function() {
    'use strict';

    function putFingerprintInForm() {
        var submit = document.getElementById('tos-submit');
        var timeZoneInput = document.getElementById('tos-time-zone');
        var fingerprintInput = document.getElementById('tos-fingerprint');
        var componentsInput = document.getElementById('tos-components-json');

        // TODO: duplicates code in detect-timezone.js
        var zoneid = "TZ_UNKNOWN";
        // Read the time zone from the Intl API.
        try {
            zoneid = Intl.DateTimeFormat().resolvedOptions().timeZone;
        } catch (noIntlAPI) {
            // Fall back to using the Date API offset from UTC.
            console.error(noIntlAPI);

            try {
                var offset = new Date().getTimezoneOffset();
                zoneid = 'UTC';
                if (offset !== 0) {
                    var minutes = offset % 60;
                    var hours = (offset - minutes) / 60;

                    // Yes, JS is backwards from the convention that everyone else uses.
                    var direction = offset < 0 ? '+' : '-';
                    zoneid += direction;

                    var absHours = Math.abs(hours);
                    if (absHours < 10) {
                        zoneid += '0';
                    }
                    zoneid += absHours;

                    var absMinutes = Math.abs(minutes);
                    if (absMinutes < 10) {
                        zoneid += '0';
                    }
                    zoneid += absMinutes;
                }
            } catch (noDateAPI) {
                console.error(noDateAPI);
            }
        }
        timeZoneInput.value = zoneid;

        try {
            new Fingerprint2().get(function(fingerprint, components) {
                try {
                    fingerprintInput.value = fingerprint;
                    componentsInput.value = JSON.stringify(components);
                } catch (cantGetFingerprint) {
                    console.error(cantGetFingerprint);
                    fingerprintInput.value = "CANT_GET_FINGERPRINT";
                    componentsInput.value = JSON.stringify(cantGetFingerprint);
                }
                submit.disabled = false;
            });
        } catch (cantInitFingerprint) {
            console.error(cantInitFingerprint);
            fingerprintInput.value = "CANT_INIT_FINGERPRINT";
            componentsInput.value = JSON.stringify(cantInitFingerprint);
            submit.disabled = false;
        }
    }

    if (document.readyState === 'complete') {
        putFingerprintInForm();
    } else {
        window.addEventListener('load', putFingerprintInForm);
    }
})();
