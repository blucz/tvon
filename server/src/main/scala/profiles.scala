package tvon.server

import java.util.Date

trait ProfileDatabaseComponent    { val db: ProfileDatabase    }
trait ProfileDatabase extends Database {
    def tryGetProfile(profileId: String): Option[DatabaseProfile]
    def putProfile(profile: DatabaseProfile)
    def loadProfiles(): List[DatabaseProfile]
    def deleteProfile(profileId: String)
}

case class DatabaseProfile(
  profileId       : String,
  name            : String,
  autoQueue       : List[AutoQueueItem]       = List(),
  explicitQueue   : List[ExplicitQueueItem]   = List(),
  explicitUnqueue : List[ExplicitQueueItem]   = List(),
  history         : List[PlayHistoryItem]     = List()
)

case class PlayHistoryItem(
  videoLink: VideoLink,
  watched:   Date
)

case class AutoQueueItem(
  path:      String,
  dateAdded: Date
)

case class ExplicitQueueItem(
  videoId:   String,
  dateAdded: Date
)

class Profile(json: DatabaseProfile) {
  val profileId       : String                  = json.profileId
  var name            : String                  = json.name
  var autoQueue       : List[AutoQueueItem]     = json.autoQueue
  var explicitQueue   : List[ExplicitQueueItem] = json.explicitQueue
  var explicitUnqueue : List[ExplicitQueueItem] = json.explicitUnqueue
  var history         : List[PlayHistoryItem]   = json.history

  def toDatabase: DatabaseProfile = {
    DatabaseProfile(
      profileId       = profileId,
      name            = name,
      autoQueue       = autoQueue,
      explicitQueue   = explicitQueue, 
      explicitUnqueue = explicitUnqueue, 
      history         = history
    )
  }

  def toApi: ApiProfile = {
    ApiProfile(
      profileId   = profileId,
      name        = name
    )
  }
}

trait ProfilesComponent extends Lifecycle { this: ProfileDatabaseComponent =>
  val profiles: Profiles = new Profiles

  override def init() {
    Log.info("[profiles] initializing")
    profiles.init()
    super.init()
  }

  class Profiles extends Lock { 
    var profiles = Map[String,Profile]()

    def allProfiles: List[Profile] = lock { profiles.values.toList }

    private def save(profile: Profile) {
      db.putProfile(profile.toDatabase)
    }

    def get(profileId: String): Option[Profile] = {
      lock {
        profiles.get(profileId)
      }
    }

    def edit(profile: Profile, name: String) {
      lock {
        profile.name = name
        save(profile)
      }
    }

    def create(name: String): Profile = lock {
      val profile = new Profile(new DatabaseProfile(profileId = Utils.newGuid, name = name))
      save(profile)
      profiles += profile.profileId -> profile
      profile
    }

    def delete(profile: Profile) {
      lock {
        db.deleteProfile(profile.profileId)
        profiles -= profile.profileId
      }
    }

    def init() {
      lock {
        for (json <- db.loadProfiles()) {
          profiles+= json.profileId -> new Profile(json)
        }
      }
    }
  }
}

case class ApiProfile(
  profileId : String,
  name      : String
)

case class ApiProfileList (
  profiles: List[ApiProfile]
)
