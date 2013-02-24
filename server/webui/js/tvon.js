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
}])

function BrowseController($scope, $rootScope) {
    $scope.levels           = []

    function refresh() {
        $scope.level    = $scope.levels[$scope.levels.length-1]
        $scope.can_back = $scope.levels.length > 1 
    }

    $scope.browse = function(item, path) {
        var data = {}
        var level = { is_loading: true, exists: false, item: item } 
        $scope.levels.push(level)
        if ($rootScope.profile != undefined) data.profileId = $rootScope.profile.profileId
        if ($rootScope.screen  != undefined) data.screenId  = $rootScope.screen.screenId
        $.ajax(GET("/api/browse/" + path, data)).done(function(data) {
            $scope.$apply(function(){ 
                $scope.level.is_loading = false
                if (data.status == "success") {
                    for (i in data.value) { $scope.level[i] = data.value[i] }
                    $scope.level.exists  = true
                    console.log($scope.level)
                }
                refresh();
            })
        })
        refresh()
    }
    $scope.pop = function() {
        $scope.levels.pop()
        refresh()
    }
    $scope.popto = function(level) {
        while ($scope.level != level) {
            $scope.levels.pop()
            refresh()
        }
    }
    $scope.doaction = function(action) {
        console.log(action)
        var data = { action: action.action }
        if ($rootScope.profile != undefined) data.profileId = $rootScope.profile.profileId
        if ($rootScope.screen  != undefined) data.screenId  = $rootScope.screen.screenId
        $.ajax(GET("/api/browse/" + action.path, data)).done(function(data) {
            $scope.$apply(function(){ 
                if (data.status == "success") {
                    for (i in data.value) { $scope.level[i] = data.value[i] }
                    console.log($scope.level)
                }
                refresh();
            })
        })
    }

    $scope.browse({ title: 'Browse'}, '')
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

