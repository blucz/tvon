package tvon.server;

trait ProfileDatabaseComponent    { val db: ProfileDatabase    }
trait ProfileDatabase extends Database {
    def tryGetProfile(profileId: String): Option[DatabaseProfile]
    def putProfile(profile: DatabaseProfile)
    def loadProfiles(): List[DatabaseProfile]
    def deleteProfile(profileId: String)
}

case class DatabaseProfile(
  profileId : String,
  name      : String
)

class Profile(json: DatabaseProfile) {
  val profileId : String = json.profileId
  var name      : String = json.name

  def toDatabase: DatabaseProfile = {
    DatabaseProfile(
      profileId   = profileId,
      name        = name
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

    private def save(profile: Profile) {
      db.putProfile(profile.toDatabase)
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
