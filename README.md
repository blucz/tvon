TV ON
-----

- discover video content automatically
- multiple user profiles
- keep db of videos including:
    - date added
    - metadata from imdb api
    - play history per profile
- exposes universe-like browse hierarchy
- web server/remote control
- select profile

Browse Hierarchy:
    
    My Queue
        TV 
        Movies
        Other
        
    All
        TV
            Grey's Anatomy
                Season 1                                            --> Add all to queue, auto-add items to queue, shuffle
                   Episode 1 - Foo Bar             <state>         --> Delete, Play, Add to Queue, Resume (if partially watched)
                    Added 1/24/2012                                 <state> = Unwatched, Watched, Queued
        Movies
            Recently Added
            All
            Decades
            Genres
            Actors
            Directors
            Countries
    
        Other
            Recently Added
            All

on "play", ask who's watching. remember setting for 4h, then default back to "just me"

Remote:
-------

    Profile: [brian v]              Screen:  [screen v]
    
    [cover]       <Title>
                  <Metadata>  
    
    [playpause] [stop] [---------seek------------------]
    
    [vol +/-] [mute] [standby]  


screen  - native on each platform
------
- configured with name of screen
- includes volume/mute control, standby

remote  - native on each platform
------
- launch web gui for now? native copy of web gui later?



