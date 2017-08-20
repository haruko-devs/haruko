(function() {
    'use strict';

    function detectAndHighlightOrInsert() {
        try {
            var zoneid = null;
            var elem = null;

            // Try to find the user's zone ID, if we can read it and it's in our list of IDs.
            try {
                zoneid = Intl.DateTimeFormat().resolvedOptions().timeZone;
            } catch (noTimeZoneAPI) {
                // Fall back to using offset from UTC.
                console.error(noTimeZoneAPI);
            }
            elem = document.getElementById(zoneid);

            // Construct a custom zone ID with the offset from UTC.
            if (elem === null) {
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

                // Insert it in a new row at the end of the table.
                var tzTableBody = document.getElementById('tztable-body');

                var row = document.createElement('tr');
                row.id = zoneid;

                var zoneidCol = document.createElement('td');

                var zoneidCode = document.createElement('code');

                var zoneidText = document.createTextNode(zoneid);
                zoneidCode.appendChild(zoneidText);

                zoneidCol.appendChild(zoneidCode);

                row.appendChild(zoneidCol);

                var descCol = document.createElement('td');

                var descText = document.createTextNode('your offset from UTC');
                descCol.appendChild(descText);

                row.appendChild(descCol);

                tzTableBody.appendChild(row);
                elem = row;
            }

            // Highlight/bold the user's current zone.
            elem.style.fontWeight = 'bold';
            elem.style.background = 'yellow';
            elem.scrollIntoView();
        } catch (unexpected) {
            console.error(unexpected);
        }
    }

    if (document.readyState === 'complete') {
        detectAndHighlightOrInsert();
    } else {
        window.addEventListener('load', detectAndHighlightOrInsert);
    }
})();
