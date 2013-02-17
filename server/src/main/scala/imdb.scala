package tvon.server

case class IMDBEpisodeJSON(
  val title            : Option[String],
  val season           : Int,
  val episode          : Int,
  val date             : String
)

case class DatabaseIMDBMetadata(
  val imdb_id          : String,
  val writers          : List[String],
  val actors           : List[String],
  val directors        : List[String],
  val `type`           : String,
  val year             : Option[Int],
  val episodes         : List[IMDBEpisodeJSON],
  val runtime          : List[String],
  val language         : List[String],
  val film_locations   : List[String],
  val imdbUrl          : Option[String],
  val plot             : Option[String],
  val plot_simple      : Option[String],
  val poster           : Option[String],
  val rating           : Option[Double],
  val rated            : Option[String],
  val rating_count     : Option[Int],
  val release_date     : Option[Int],
  val country          : List[String]
)

object IMDB {
}
