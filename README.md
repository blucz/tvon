TVon
-----

TVon is a suite of software that is meant to take over your TV and provide convenient video content management
and remote control. It works best if you acquire content in the form of video files (.avi, .mkv, etc)

- TVon Server runs somewhere on your home network and watches folders that contain video content
- TVon Screen runs on your HTPC and handles presentation of video content
- TVon Remote runs on each of your mobile devices and allows for remote control
- remote control is also possible via a web interface running as part of TVon Server
- TVon has extension points to support stuff like IR control + automation

TVon is a work in progress. Not all of these components have been implemented yet, and it is not currently
usable.


The Plan
--------

- discover video content automatically
- support multiple user profiles
- keep db of previously discovered videos including:
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



