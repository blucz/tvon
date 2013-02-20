// 
// Angular setup
//
var filters    = angular.module('tvon.filters', [])
var directives = angular.module('tvon.directives', [])
var app        = angular.module('tvon.app', ['tvon.filters', 'tvon.directives']);

filters.filter('join', function () {
    return function(list, joiner) {
        if (joiner == undefined) joiner = ", "
        if (list   == undefined) list   = []
        return list.join(joiner)
    }
});

//
// Custom directives
//
directives.directive('openDialog', function(){
    return {
        restrict: 'A',
        link: function(scope, elem, attr, ctrl) {
            var dialogId = '#' + attr.openDialog;
            elem.bind('click', function(e) {
                $(dialogId).dialog_show();
            });
        }
    };
});

directives.directive('doAutoFocus', [function() {
    return {
        restrict: 'A',
        link: function(scope, elm, attrs, model) {
            $(elm).watch('display', function() {
                if ($(elm).attr("display") != "none") {
                    setTimeout(function() { $(elm).find("[autofocus]").focus(); }, 50)
                }
            });
        }
    };
}]);

directives.directive('centeredDialog', [function() {
    return {
        restrict: 'E', // element only
        replace: true, // This HTML will replace the centeredDialog directive.
        transclude: true,
        scope: { title: '@dialogTitle' },
        template: '<div class="centereddialogbg">' +
                        '<div class="centereddialogmain">' +
                            '<div ng-show="title == undefined" class="dialogtitlenobar"><div class="dialogtitlex" onclick="$(this).parents(\'.centereddialogbg\').jqmHide();">&#10006;</div></div>' +
                            '<div ng-show="title != undefined" class="dialogtitle"><div class="dialogtitlex" onclick="$(this).parents(\'.centereddialogbg\').jqmHide();">&#10006;</div>{{title}}</div>' +
                            '<div class="dialogbody" ng-transclude>' +
                            '</div>' +
                        '</div>' +
                    '</div>',
        compile: function(element, attrs, transclude) {
            var dialog  = $(element);
            var main = $(element).find(".centereddialogmain");

            var maxheight = attrs["dialogMaxHeight"] ||  "";
            var minheight = attrs["dialogMinHeight"] ||  "";
            var maxwidth = attrs["dialogMaxWidth"] ||  "";
            var minwidth = attrs["dialogMinWidth"] ||  "";
            var height = attrs["dialogHeight"] ||  "";
            var width = attrs["dialogWidth"] ||  "";

            main.css("width", width);
            main.css("height", height);
            main.css("min-width", minwidth);
            main.css("min-height", minheight);
            main.css("max-width", maxwidth);
            main.css("max-height", maxheight);

            return function(scope, element, attrs) {
                $(element).find(".centereddialogmain").click(function(ev) {
                    ev.stopPropagation();
                });
                $(element).click(function(ev) {
                    ev.stopPropagation();
                    $(element).jqmHide();
                });
                angular.element($(element).find(".dialogbody").children()[0]).scope().dialog_close = function() {
                    $(element).jqmHide();
                }
            }
        },
    };
}]);

(function($){
    $.fn.dialog_set_data = function(name, val) {
        return this.each(function() {
            angular.element($(this).find(".dialogbody").children()[0]).scope()[name] = val;
        });
    };
    $.fn.dialog_set_title = function(title) {
        return this.each(function() {
            angular.element($(this).find(".dialogbody")).scope().title = title;
        });
    };
    $.fn.dialog_show = function() {
        return this.each(function() {
            $(this).jqm();
            $(this).jqmShow();
            $(this).find(".dialogbody").scrollTop(0);
        });
    };
    $.fn.dialog_close = function() {
        return this.each(function() {
            $(this).jqmHide();
        });
    };
    $.fn.confirm_dialog_init = function(title, text, buttons) {
        var b = $.extend(true, [], buttons);
        for (i in b) {
            if (b[i].action == undefined) {
                b[i].action = function() { $("#dialog_confirm").dialog_close(); }
            } else {
                var oldfunc = b[i].action;
                b[i].action = function() { $("#dialog_confirm").dialog_close(); oldfunc(); }
            }
        }
        $("#dialog_confirm").dialog_set_title(title);
        $("#dialog_confirm").dialog_set_data("text", text);
        $("#dialog_confirm").dialog_set_data("buttons", b);
        $("#dialog_confirm").dialog_show();
    };

})(jQuery);

//
// Other utils
//
function POST(url, data) {
    return {
        type: "POST",
        url:  url,
        data: data
    }
}
function GET(url, data) {
    return {
        type: "GET",
        url:  url,
        data: data
    }
}

