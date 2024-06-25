ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"

lazy val root = (project in file("."))
  .settings(
    name := "argo",
    // Scaladocの設定
    Compile / doc / scalacOptions ++= Seq(
      "-doc-title",
      "Argo Project",
      "-doc-version",
      version.value,
      "-doc-source-url",
      "https://github.com/your-repo/argo/tree/main€{FILE_PATH}.scala",
      "-doc-canonical-base-url",
      "https://your-project-docs.com"
    ),

    // デフォルトの出力ディレクトリを指定
    Compile / doc / target := target.value / "docs",
    // その他の有用な設定
    organization := "com.yourorganization",
    homepage := Some(url("https://your-project-homepage.com")),
    licenses += ("Apache-2.0", url(
      "http://www.apache.org/licenses/LICENSE-2.0"
    )),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/your-repo/argo"),
        "scm:git:git@github.com:your-repo/argo.git"
      )
    ),
    developers := List(
      Developer(
        id = "your-id",
        name = "Your Name",
        email = "your-email@example.com",
        url = url("https://your-profile-page.com")
      )
    ),
    // プロジェクトの設定
    scalafmtOnCompile := true,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
  )
