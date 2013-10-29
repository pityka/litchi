name := "leachi-web"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
   "pityu" %% "commons" % "4.1.1" exclude("org.specs2", "specs2_2.10.0-RC1"),
   cache
)     

lazy val MyResolvers = Seq(
   Resolver.file("LocalIvy", file(Path.userHome +"/.ivy2/local"))(Resolver.ivyStylePatterns),
   "LocalMaven" at "file:///"+Path.userHome+"/.ivy2/local",
   "Cloudbees Private" at "https://repository-pityka.forge.cloudbees.com/snapshot/",
   "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
   "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
   "Twitter" at "http://maven.twttr.com/",
   "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
   )

resolvers ++= MyResolvers

credentials += {
      val credsFile = (Path.userHome / ".ivy2" / ".credentials")
      (if (credsFile.exists) Credentials(credsFile)
       else {
        val username = System.getenv("CLOUDBEES_USER")
        val password = System.getenv("CLOUDBEES_PSW")
        val host = System.getenv("CLOUDBEES_HOST")
        val realm = System.getenv("CLOUDBEES_REALM")
        Credentials(realm,host,username,password)})
    }

play.Project.playScalaSettings

scalariformSettings
