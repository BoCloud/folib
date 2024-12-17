package com.veadan.folib.controllers.unicom;

import com.veadan.folib.artifact.coordinates.GoArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;

/**
 * @author huayanjun
 * @since 2024-12-11 10:27
 */
public class EmailTemplate {


    public static final String GROUP_LINK="{group_link}";
    public static final String USERNAME= "{username}";
    public static final String PASSWORD= "{password}";
    public static final String LOCAL_LINK="{local_links}";
    public static final String REPOSITORY_ID="{repositoryId}";
    public static final String RELEASE_REPO="{releaseRepo}";
    public static final String SNAPSHOT_REPO="{snapshotRepo}";
    public static final String BASE64_PASSWORD="{basePassword}";



    private static final String GROUP_TEMPLATE = "下载地址: {group_link}\n";
    private static final String USER_INFO_TEMPLATE = "明文账户密码\n账号:{username}\n密码:{password}\n";
    private static final String PRIVATE_TEMPLATE = "私有仓库地址:{local_links}\n";
    private static final String DEFAULT_CONTENT = GROUP_TEMPLATE + USER_INFO_TEMPLATE + PRIVATE_TEMPLATE;


    private static final String MAVEN_SETTING_FRAGMENT = "Maven setting配置:\n" +
            "<settings>\n" +
            "    <mirrors>\n" +
            "        <mirror>\n" +
            "            <id>{repositoryId}</id>\n" +
            "            <name>{repositoryId}</name>\n" +
            "            <url>{group_link}</url>\n" +
            "            <mirrorOf>*</mirrorOf>\n" +
            "        </mirror>\n" +
            "    </mirrors>\n" +
            "    <servers>\n" +
            "        <server>\n" +
            "            <id>{repositoryId}</id>\n" +
            "            <username>{username}</username>\n" +
            "            <password>{password}</password>\n" +
            "        </server>\n" +
            "    </servers>\n" +
            "</settings>\n";
    private static final String MAVEN_POM_FRAGMENT = "pom.xml配置 \n" +
            "<distributionManagement>\n" +
            "  <repository>\n" +
            "    <id>releases</id>\n" +
            "    <name>releases</name>\n" +
            "    <url>{releaseRepo}</url>\n" +
            "  </repository>\n" +
            "  <snapshotRepository>\n" +
            "    <id>snapshots</id>\n" +
            "    <name>snapshots</name>\n" +
            "    <url>{snapshotRepo}</url>\n" +
            "  </snapshotRepository>\n" +
            "</distributionManagement>\n";

    private static final String MAVEN_COMMAND = "编译 mvn clean install\n上传mvn deploy\n";

    private static final String MAVEN_CONTENT = "MAVEN源\n" + DEFAULT_CONTENT + MAVEN_SETTING_FRAGMENT + MAVEN_POM_FRAGMENT+MAVEN_COMMAND;

    private static final String GRADLE_DOWN_FRAGMENT = "下载依赖build.gradle添加\n" +
            "  repositories {\n" +
            "    maven {\n" +
            "      url '{group_link}'\n" +
            "    }\n";

    private static final String GRADLE_DEPLOY_FRAGMENT = "发布依赖build.gradle添加:\n" +
            "plugins {\n" +
            "    id 'java'\n" +
            "    id 'maven-publish'\n" +
            "}\n" +
            "\n" +
            "group = 'com.example'\n" +
            "version = '1.0.0-SNAPSHOT' // 动态改变此版本\n" +
            "\n" +
            "publishing {\n" +
            "    publications {\n" +
            "        mavenJava(MavenPublication) {\n" +
            "            from components.java\n" +
            "        }\n" +
            "    }\n" +
            "    repositories {\n" +
            "        maven {\n" +
            "            def isSnapshot = version.endsWith(\"-SNAPSHOT\")\n" +
            "            url = isSnapshot ? uri(\"{snapshotRepo}\") : uri(\"{releaseRepo}\")\n" +
            "            credentials {\n" +
            "                username = {username}\n" +
            "                password = {password}\n" +
            "            }\n" +
            "        }\n" +
            "    }\n" +
            "}\n";

    private static final String GRADLE_COMMAND = "编译 ./gradlew build \n上传 ./gradlew publish\n";

    private static final String GRADLE_CONTENT = "gradle源:\n" + DEFAULT_CONTENT + GRADLE_DOWN_FRAGMENT + GRADLE_DEPLOY_FRAGMENT + GRADLE_COMMAND;


    private static final String NPM_REPO = "需要在项目的根目录.npmrc文件并填入如下:\n" +
            "registry={group_link}\n" +
            "publishConfig.registry={local_links}\n" +
            "//{local_links}:username={username}\n" +
            "//{local_links}:password={basePassword}\n" +
            "//{local_links}:email=user@example.com\n";

    private static final String NPM_COMMAND = "下载 npm install \n上传 npm publish\n";

    private static final String NPM_CONTENT = "npm源:\n" + DEFAULT_CONTENT + NPM_REPO + NPM_COMMAND;

    private static final String PYPI_REPO = "在用户的主目录下的.pypirc文件:\n"
            + "[distutils]\n" +
            "index-servers =\n" +
            "    group\n" +
            "    local\n" +
            "\n" +
            "[group]\n" +
            "repository: {group_link}\n" +
            "\n" +
            "[local]\n" +
            "repository: {local_links}\n" +
            "username: {username}\n" +
            "password: {password}\n";

    private static final String PYPI_COMMAND = "下载 pip install <package_name> \n  python3 -m twine upload -r local dist/* \n";

    private static final String PYPI_CONTENT = "pypi源:\n" + DEFAULT_CONTENT + PYPI_REPO + PYPI_COMMAND;


    private static final String GO_REPO = "可以设置环境变量:\n+" +
            "set GOPROXY={group_link}\n";

    private static final String GO_CONTENT = "GO源:\n" + DEFAULT_CONTENT + GO_REPO;


    public static String getTemplateByLayout(String layout, String subLayout) {
        switch (layout) {
            case MavenArtifactCoordinates.LAYOUT_NAME:
                if ("gradle".equals(subLayout)) {
                    return GRADLE_CONTENT;
                } else {
                    return MAVEN_CONTENT;
                }
            case NpmArtifactCoordinates.LAYOUT_NAME:
                return NPM_CONTENT;
            case GoArtifactCoordinates.LAYOUT_NAME:
                return GO_CONTENT;
            case PypiArtifactCoordinates.LAYOUT_NAME:
                return PYPI_CONTENT;
            default:
                return layout+"源\n"+DEFAULT_CONTENT;
        }
    }


}
