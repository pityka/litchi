import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

    val appName         = "leachi-web"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "pityu" %% "commons" % "2",
      "de.erichseifert.gral" %% "gral" % "0.9-SNAPSHOT-pityu3b5",
      "de.erichseifert.gral" %% "vectorgraphics2d" % "0.9.1-pityu1b3"
      
    )
   



    val main = play.Project(appName, appVersion, appDependencies).settings(
      // Add your own project settings here    
    resolvers +=  "Cloudbees Private" at "https://repository-pityka.forge.cloudbees.com/snapshot/",
    resolvers +=   "Twitter" at "http://maven.twttr.com/",
    credentials += {
      val credsFile = (Path.userHome / ".ivy2" / ".credentials")
      (if (credsFile.exists) Credentials(credsFile)
       else {
        val username = System.getenv("CLOUDBEES_USER")
        val password = System.getenv("CLOUDBEES_PSW")
        val host = System.getenv("CLOUDBEES_HOST")
        val realm = System.getenv("CLOUDBEES_REALM") 
        println(host)
        println(realm)       
        Credentials(realm,host,username,password)})
    }
          
    )

}
