package tvon.server;

import scala.collection.mutable._
import java.util.UUID

case class ProfileJSON(
  profileId : String,
  name      : String
)

class Profile(profiles: Profiles, json: ProfileJSON) {
  val profileId : String = json.profileId
  var name      : String = json.name

  def toJSON: ProfileJSON = {
    ProfileJSON(
      profileId   = profileId,
      name        = name
    )
  }
}

class Profiles(val manager: Manager) {
  val profiles = new HashMap[String,Profile]

  private def save(profile: Profile) {
    manager.db.putProfile(profile.toJSON)
  }

  def edit(profile: Profile, name: String) {
    profile.name = name
    save(profile)
  }

  def create(name: String): Profile = {
    val profile = new Profile(this, new ProfileJSON(profileId = UUID.randomUUID.toString, name = name))
    save(profile)
    profiles(profile.profileId) = profile
    profile
  }

  def delete(profile: Profile) {
    manager.db.deleteProfile(profile.profileId)
  }

  def load() {
    for (json <- manager.db.loadProfiles()) {
      profiles(json.profileId) = new Profile(this, json)
    }
  }
}


