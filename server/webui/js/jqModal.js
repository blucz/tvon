
/*
 * This was based on jqModal, but looks nothing like the original anymore.
 * Original message is below:
 *
 * --
 *
 * jqModal - Minimalist Modaling with jQuery
 *
 * Copyright (c) 2007 Brice Burgess <bhb@iceburg.net>, http://www.iceburg.net
 * Licensed under the MIT License:
 * http://www.opensource.org/licenses/mit-license.php
 * 
 * $Version: 2007.??.?? +r12 beta
 * Requires: jQuery 1.1.3+
 */
(function ($) {
    /**
     * Initialize a set of elements as "modals". Modals typically are popup dialogs,
     * notices, modal windows, and image containers. An expando ("_jqm") containing
     * the UUID or "serial" of the modal is added to each element. This expando helps
     * reference the modal's settings in the jqModal Hash Object (jQuery.jqm.hash)
     * 
     * Accepts a parameter object with the following modal settings;
     * 
     * (Integer) zIndex - Desired z-Index of the modal. This setting does not override (has no effect on) preexisting z-Index styling (set via CSS or inline style).  
     * (Integer) overlay - [0-100] Translucency percentage (opacity) of the body covering overlay. Set to 0 for NO overlay, and up to 100 for a 100% opaque overlay.  
     *
     * @param Map options User defined settings for the modal(s).
     */
    $.fn.jqm = function (p) {
        var options = {
            zIndex: 3000,
            overlay: 50,
        };

        // For each element (aka "modal") $.jqm() has been called on;
        //  IF the _jqm expando exists, return (do nothing)
        //  ELSE increment serials and add _jqm expando to element ("serialization")
        //    *AND*...
        return this.each(function () {
            if (this._jqm) return;
            this._jqm = ++seq_id;

            // ... Add this element's serial to the jqModal Hash Object 
            //  Hash is globally accessible via jQuery.jqm.hash. It consists of;
            //   c: {obj} config/options
            //   a: {bool} active state (true: active/visible, false: inactive/hidden)
            //   w: {JQ DOM Element} The modal element (window/dialog/notice/etc. container)
            //   id: {int} The serial number of this modal (same as "H[id].w[0]._jqm")
            // *AND* ...
            H[seq_id] = {
                c: $.extend(options, p),
                a: false,
                w: $(this).addClass('jqmID' + seq_id),
                id: seq_id
            };

        });
    };

    // Hide/Show a modal -- first check if it is already shown or hidden via the toggle state (H[{modal serial}].a)
    $.fn.jqmShow = function () {
        return this.each(function () {
            !H[this._jqm].a && $.jqm.open(this._jqm)
        });
    };
    $.fn.jqmHide = function () {
        return this.each(function () {
            H[this._jqm].a && $.jqm.close(this._jqm)
        });
    };
    $.fn.jqmHideAll = function () {
        return $.each(H, function (i, h) {
            h.a && $.jqm.close(i)
        });
    };

    $.jqm = {
        hash: {},

        // Function is executed by $.jqmShow to show a modal
        // id: {INT} serial of modal

        // set local shortcuts
        //  h: {obj} this Modal's "hash"
        //  c: {obj} (h.c) config/options
        //  z: {INT} z-Index of Modal. If the Modal (h.w) has the z-index style set it will use this value before defaulting to the one passed in the config (h.c.zIndex)
        //  dimlayer: The overlay object
        // mark this modal as active (h.a === true)
        open: function (id) {
            var h = H[id];
            var c = h.c;

            var z = c.zIndex;
            if (activelist.length != 0) {
                z = H[activelist[activelist.length-1]].zindex + 100;
            }

            h.zindex = z;

            var dimlayer = $('<div></div>');

            c.zIndex += 100;

            h.a = true;
            h.w.css('z-index', z+1);

            !activelist[0] && hookevents('bind');
            activelist.push(id);

            //    Attach a Close event to overlay to hide modal when overlay is clicked.
            if (activelist.length == 1) {
                dimlayer.css({
                    "background-color": "#000",
                    position: 'fixed',
                    top: 0,
                    bottom: 0,
                    left: 0,
                    right: 0,
                    'z-index': z,
                    opacity: c.overlay / 100,
                });
//                dimlayer.click(function() { h.w.jqmHide(); });
                h.dimlayer = dimlayer.prependTo('body');
            } else {
//                dimlayer.css({
//                    position: 'fixed',
//                    left: 0,
//                    right: 0,
//                    top: 0,
//                    bottom: 0,
//                    top: 0,
//                    'z-index': z,
//                    opacity: 0.001
//                });
//                h.dimlayer = dimlayer.prependTo('body');
            }

            h.w.css("display", "-webkit-box");
            h.w.focus();
            h.w.find('[autofocus]').focus()
            return false;
        },

        // Function is executed by $.jqmHide to hide a modal
        // mark this modal as inactive (h.a === false)
        close: function (id) {
            var h = H[id];
            h.a = false;

            activelist.pop();
            !activelist[0] && hookevents('unbind');

            // hide (make invisible) the modal and remove the overlay.
            h.w.hide() && h.dimlayer && h.dimlayer.remove()
            return false;
        }
    };

    // set jqModal scope shortcuts;
    //  id: {INT} serials placeholder
    //  H: {HASH} shortcut to jqModal Hash Object
    //  activelist: {ARRAY} Array of active/visible modals
    var seq_id = 0;
    var H = $.jqm.hash;
    var activelist = [];

    var hookevents = function (t) {
        $(document)[t]("keydown", ev_keypress);
        $(document)[t]("mousedown", ev_mousedown);
    };

    var keepfocus = function(h, e, iskey) {
        if ($(e.target).closest('.jqmID' + h.id).length > 0) {

            // save the list of inputs
            var inputs = $(':tabbable:first, :tabbable:last', h.w);

            if (inputs.length === 0) {
                e.preventDefault();
                h.w.focus();
                h.w.find(':tabbable:first').focus()
                return false;

            } else {
                // if it's the first or last tabbable element, refocus
                if ((!e.shiftKey && e.target === inputs[inputs.length -1]) || (e.shiftKey && e.target === inputs[0])) {
                    e.preventDefault();
                    inputs[e.shiftKey ? inputs.length-1 : 0].focus();
                    return false;
                }
            }

        } else {
            e.preventDefault();
            h.w.focus();
            h.w.find(':tabbable:first').focus()
            return false;
        }
        return true;
    }

    var ev_keypress = function (e) {
        var h = H[activelist[activelist.length - 1]];

        if (e.keyCode == 27) {
            $.jqm.close(h.id)
            return true;
        }
        if (e.keyCode == 9) {
            return keepfocus(h, e, true)
        }

        return true;
    };

    var ev_mousedown = function (e) {
        var h = H[activelist[activelist.length - 1]];
        h.w.focus();
        return true;
    };
})(jQuery);
