app.run(["$rootScope", function($rootScope) {
    $rootScope.profiles = []
    $rootScope.profile  = undefined

    // save selectedprofileid to local storage when it changes
    $rootScope.$watch('profile', function() {
        if ($rootScope.profile)
            localStorage.selectedProfileId = $rootScope.profile.profileId;
    })

    // load profiles
    function loadProfiles(profiles) {
        $rootScope.profiles = []
        $rootScope.profile  = profiles[0]
        profiles.forEach(function(p){
            $rootScope.profiles.push(p)
            if (p.profileId == localStorage.selectedProfileId)
                $rootScope.profile = p
        })
    }
    $.ajax("/api/profiles").done(function(data) { 
        $rootScope.$apply(function() {
            loadProfiles(data.value.profiles)
        })
    })

    $("#tabs").tabs()
}])

function BrowseController($scope, $rootScope) {
    $scope.levels        = [{exists:false}]
    $scope.level         = $scope.levels[$scope.levels.length-1]

    function refresh() {
        if ($scope.levels.length == 0) $scope.levels.push({exists:false})
        $scope.level    = $scope.levels[$scope.levels.length-1]
        $scope.can_back = $scope.levels.length > 1
    }

    $scope.browse = function(path) {
        var data = {}
        if ($rootScope.profile != undefined) data.profileId = $rootScope.profile.profileId
        if ($rootScope.screen  != undefined) data.screenId  = $rootScope.screen.screenId
        $.ajax(GET("/api/browse/" + path, data)).done(function(data) {
            $scope.$apply(function(){ 
                if (data.status == "success") {
                    while ($scope.levels.length > 0 && !$scope.levels[$scope.levels.length-1].exists) {
                        $scope.levels.pop()
                    }
                    data.value.exists = true
                    $scope.levels.push(data.value)
                } else {
                    if ($scope.level.exists) {
                        $scope.levels.push({exists:false})
                    }
                }
                refresh();
            })
        })
    }
    $scope.pop = function() {
        $scope.levels.pop()
        refresh()
    }

    $scope.browse('')
    refresh();
}

function ControlController($scope, $rootScope) {
}

function MainController($scope, $rootScope) {
    $scope.mode = "remote"
    $scope.remote = function() {
        $scope.mode = "remote"
    }
    $scope.browse = function() {
        $scope.mode = "browse"
    }
}

function AddProfileController($scope, $rootScope) {
    $scope.name = ""
    $scope.create = function(name, cb) {
        cb()
        $scope.name = ""
        $.ajax(POST("/api/profiles/create", { name: name })).done(function(data) {
            if (data.status == "success") {
                $rootScope.$apply(function() {
                    $rootScope.profiles.push(data.value)
                    $rootScope.profile = data.value
                })
            } else {
                alert("Couldn't create profile: " + data.status)
            }
        })
    }
}
