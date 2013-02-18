package tvon.server;

case class DatabaseProfile(
  profileId : String,
  name      : String
)

class Profile(profiles: Profiles, json: DatabaseProfile) {
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

class Profiles(val manager: Manager) {
  var profiles = Map[String,Profile]()

  private def save(profile: Profile) {
    manager.db.putProfile(profile.toDatabase)
  }

  def edit(profile: Profile, name: String) {
    profile.name = name
    save(profile)
  }

  def create(name: String): Profile = {
    val profile = new Profile(this, new DatabaseProfile(profileId = Utils.newGuid, name = name))
    save(profile)
    profiles += profile.profileId -> profile
    profile
  }

  def delete(profile: Profile) {
    manager.db.deleteProfile(profile.profileId)
    profiles -= profile.profileId
  }

  def load() {
    for (json <- manager.db.loadProfiles()) {
      profiles+= json.profileId -> new Profile(this, json)
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
