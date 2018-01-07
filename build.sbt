name := """Main"""

//scalaVersion := "2.12.1"
scalaVersion := "2.11.7"

//scalacOptions ++= Seq("-unchecked","-deprecation","-feature")
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

//libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
//libraryDependencies += "org.apache.spark" %% "spark-core" % "2.1.0"
//libraryDependencies += "org.apache.spark" %% "spark-mllib" % "2.1.0"
libraryDependencies += "com.datumbox" % "libsvm" % "3.22"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"
// https://mvnrepository.com/artifact/nu.validator.htmlparser/htmlparser
//libraryDependencies += "nu.validator.htmlparser" % "htmlparser" % "1.4"

// https://mvnrepository.com/artifact/org.scala-lang/scala-xml
//libraryDependencies += "org.scala-lang" % "scala-xml" % "2.11.0-M4"

// https://mvnrepository.com/artifact/net.sourceforge.htmlcleaner/htmlcleaner
//libraryDependencies += "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.19"

// https://jsoup.org/
libraryDependencies += "org.jsoup" % "jsoup" % "1.11.2"

//resolvers += Resolver.sonatypeRepo("public")
