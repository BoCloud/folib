<template>
  <div>
    <a-drawer
      placement="right"
      width="65%"
      title="使用说明"
      :visible="usedVisible"
      @close="closeUsedVisibleDialog"
    >
      <a-timeline v-if="repositoryType === 'maven'">
        <a-timeline-item color="primary">
          Maven全局配置
          <small>maven settings配置</small>
          <p>你需要复制以下配置到你的maven的/conf/settings.xml中</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              '<mirror>\n' +
              '   <id>' +
              folibRepository.id +
              '</id>\n' +
              '   <name>' +
              folibRepository.id +
              '</name>\n' +
              '   <url>' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '</url>\n' +
              '   <mirrorOf>*</mirrorOf>\n' +
              '</mirror>'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          POM配置
          <small>pom.xml配置</small>
          <p>
            通常需要在pom.xml中进行指定上传的配置，和常用的maven仓库一样使用,具体pom配置可参阅：https://maven.apache.org/pom.html
          </p>
          <p>
            注意：本仓库类型为:<strong>{{
              folibRepository.type === "proxy"
                ? "代理库"
                : folibRepository.type === "group"
                ? "组合库"
                : "本地库"
            }}</strong
            >{{
              folibRepository.type === "proxy"
                ? "不支持上传"
                : folibRepository.type === "group"
                ? "不支持上传"
                : "可以上传"
            }}
          </p>
          <prism-editor
            class="my-editor height-300"
            :value="
              '<repositories>\n' +
              '   <repository>\n' +
              '      <id>' +
              folibRepository.id +
              '</id>\n' +
              '      <url>' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '</url>\n' +
              '   </repository>\n' +
              '</repositories>\n' +
              '\n' +
              '<distributionManagement>\n' +
              '   <repository>\n' +
              '      <id>' +
              folibRepository.id +
              '</id>\n' +
              '      <url>' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '</url>\n' +
              '   </repository>\n' +
              '</distributionManagement>'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>maven 通常使用命令</small>
          <p>
            和通常maven一样使用，具体参阅：https://maven.apache.org/index.html
          </p>

          <prism-editor
            class="my-editor height-300"
            :value="'mvn clean intall\n' + 'mvn clean deploy'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'gradle'">
        <a-timeline-item color="primary">
          Gradle配置
          <small>Gradle配置仓库</small>
          <p>你需要在 build.gradle 文件中加入以下代码:</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'allprojects {\n' +
              '  repositories {\n' +
              '    maven {\n' +
              '      url \'' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\'\n' +
              '    }\n' +
              '    mavenLocal()\n' +
              '    mavenCentral()\n' +
              '  }\n' +
              '}'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>Gradle 通常使用命令</small>
          <p>
            和通常Gradle一样使用，具体参阅：https://docs.gradle.org/current/userguide/userguide.html
          </p>

          <prism-editor
            class="my-editor height-300"
            :value="'gradle dependencies \n' + './gradlew dependencies '"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'sbt'">
        <a-timeline-item color="primary">
          SBT配置
          <small>SBT配置仓库</small>
          <p>你需要编辑或新建 ${HOME}/.sbt/repositories，文件中加入以下代码:</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              '[repositories]\n' +
              'local\n' +
              '' +
              folibRepository.id +
              ': ' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              ''
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          全局配置
          <small>SBT全局配置</small>
          <p>
            编辑 ${sbt_安装目录}/conf/sbtconfig.txt，如果你使用的 idea，在
            settings->SBT-> jvm parameters 添加
          </p>

          <prism-editor
            class="my-editor height-300"
            :value="'-Dsbt.override.build.repos=true ## 忽略工程自定义的 resolvers，采用全局配置\n'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          使用命令操作
          <small>SBT命令使用</small>
          <p>和通常SBT命令一样使用，具体参阅：https://www.scala-sbt.org/</p>

          <prism-editor
            class="my-editor height-300"
            :value="'sbt compile publish'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'ivy'">
        <a-timeline-item color="primary">
          ivy配置
          <small>ivy配置仓库</small>
          <p>
            你需要修改 ${USER_HOME}/.ivy2/ivysettings.xml，文件中加入以下代码:
          </p>

          <prism-editor
            class="my-editor height-300"
            :value="ivyCode"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          使用命令操作
          <small>ant-ivy命令使用</small>
          <p>
            和通常SBT命令一样使用，具体参阅：https://ant.apache.org/ivy/history/2.4.0/use/makepom.html
          </p>

          <prism-editor
            class="my-editor height-300"
            :value="'ant build deploy'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'npm'">
        <a-timeline-item color="primary">
          NPM全局配置
          <small>NPM配置全局配置</small>
          <p>你可以全局配置npm的mirror,操作如下:</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'npm config set registry ' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              '\n' +
              'npm config list #查看npm当前配置'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          工程配置
          <small>该方式只对当前工程生效</small>
          <p>需要在仓库下创建.npmrc文件并填入如下：</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'registry=' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              'always-auth=true\n' +
              'email=yours4@example.com\n' +
              '_auth=YWRtaW46cGFzc3dvcmQ=\n' +
              '\n' +
              '; `_auth` 是 base64 的token\n' +
              '; 你也可以采用用户名密码模式:\n' +
              '; username=admin\n' +
              '; _password=password'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>NPM 通常使用命令</small>
          <p>和通常NPM一样使用，具体参阅：https://npmjs.org/</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'npm install   #安装依赖\n' + '\n' + 'npm publish  #上传依赖'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'rpm'">
        <a-timeline-item color="primary">
          RPM配置
          <small>centOS yum源配置</small>
          <p>
            在/etc/yum.repos.d/中添加一个local_test.repo文件,镜像服务器为阿里云,操作如下:
          </p>

          <prism-editor
            class="my-editor height-300"
            :value="
              '[local_test]' +
              '\n' +
              'name=CentOS-$releasever - Base - mirrors.aliyun.com' +
              '\n' +
              'enabled=1' +
              '\n' +
              'baseurl=' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '/' +
              '\n' +
              'gpgcheck=0'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>yum 使用命令</small>
          <p>仅供参考，详情请查相关文档</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'yum clean all #清除YUM缓存' +
              '\n' +
              'yum repolist #显示所有仓库' +
              '\n' +
              'yum install --downloadonly --downloaddir=/folib_test/mysql mysql #拉mysql 相关rpm包到/folib_test/mysql 目录下'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'helm'">
        <a-timeline-item color="primary">
          Helm配置
          <p>将folib helm仓添加到本地操作步骤</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'helm  registry  login  ' +
              baseUrl +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              '\n' +
              'helm  repo  add   ' +
              folibRepository.id +
              '   ' +
              baseUrl +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          上传Chart包到Helm仓库
          <p>安装 helm-cm-push插件</p>
          <prism-editor
            class="my-editor height-300"
            :value="
              '1.   https://github.com/chartmuseum/helm-push/releases 下载各个系统下的 helm-cm-push 安装包' +
              '\n' +
              '2.   把安装包复制到 helm 的plugins目录下解压     ' +
              '\n' +
              '\n' +
              '\n' +
              '\n' +
              'helm-cm-push 命令上传' +
              '\n' +
              '\n' +
              '1. 进入 helm-cm-push plugins 插件bin目录       #helm env 查看plugins目录位置' +
              '\n' +
              '\n' +
              '2.   执行上传' +
              '\n' +
              '例如 ：上传/app/fluentd-4.5.2.tgz 的chart包 到' +
              folibRepository.id +
              '\n' +
              '\n' +
              './helm-cm-push  /app/fluentd-4.5.2.tgz  ' +
              folibRepository.id +
              '\n' +
              '\n' +
              '参数说明：第一个参数是cahrt 包全路径   第二个参数是加入到本地的helm 仓库名. --username  --password 可选鉴权使用' +
              '\n'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          helm 使用常用命令
          <p>详细使用参考官网 https://helm.sh/zh/docs/intro/using_helm/</p>
          <prism-editor
            class="my-editor height-300"
            :value="
              'helm reop update  #更新本地仓库' +
              '\n' +
              '\n' +
              'helm search repo mysql     #搜索本地的mysql charts' +
              '\n' +
              '\n' +
              'helm pull  ' +
              folibRepository.id +
              '/mysql   ./    #将最新的mysql 下载到本地  --version 可指定版本' +
              '\n'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'conan'">
        <a-timeline-item color="primary">
          Conan配置
          <p>将folib conan仓添加到本地操作步骤</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'conan remote add   ' +
              folibRepository.id +
              '   ' +
              baseUrl +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '   false\n' +
              '\n' +
              'conan user -p [password] -r a_local_conan [username]   #添加访问用户名密码'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Conan包 相关操作
          <p></p>
          <prism-editor
            class="my-editor height-300"
            :value="
              '1.   搜索本地已有的Conan' +
              '\n' +
              '\n' +
              'conan  search ' +
              '\n' +
              '2.   上传本地包到   ' +
              folibRepository.id +
              '\n' +
              '\n' +
              '例如上传 zulu-openjdk/11.0.15 ' +
              '\n' +
              'conan  upload  zulu-openjdk/11.0.15@ -r ' +
              folibRepository.id +
              '  --all' +
              '\n' +
              '\n' +
              '3.   下载与搜索   ' +
              folibRepository.id +
              '\n' +
              '\n' +
              '例如下载 ' +
              folibRepository.id +
              '  zulu-openjdk/11.0.15 ' +
              '\n' +
              '\n' +
              'conan   search    zulu-openjdk -r   ' +
              folibRepository.id +
              '\n' +
              'conan   download   zulu-openjdk/11.0.15@    -r   ' +
              folibRepository.id +
              '\n'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          Conan 使用常用命令
          <p>
            详细使用参考官网
            https://docs.conan.io/en/latest/reference/commands.html
          </p>
          <prism-editor
            class="my-editor height-300"
            :value="'conan  remote list  #查询已加入的仓库' + '\n'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'yarn'">
        <a-timeline-item color="primary">
          Yarn配置
          <small>Yarn配置全局配置</small>
          <p>你可以全局配置Yarn的mirror,操作如下:</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'yarn config set registry ' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              '\n' +
              'yarn config get registry #查看npm当前配置'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          工程配置
          <small>该方式只对当前工程生效</small>
          <p>需要在仓库下创建.npmrc文件并填入如下：</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'registry=' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              'always-auth=true\n' +
              'email=yours4@example.com\n' +
              '_auth=YWRtaW46cGFzc3dvcmQ=\n' +
              '\n' +
              '; `_auth` 是 base64 的token\n' +
              '; 你也可以采用用户名密码模式:\n' +
              '; username=admin\n' +
              '; _password=password'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>Yarn 通常使用命令</small>
          <p>和通常Yarn一样使用，具体参阅：https://npmjs.org/</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'yarn install   #安装依赖\n' + '\n' + 'yarn publish  #上传依赖'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'pypi'">
        <a-timeline-item color="primary">
          Pypi配置
          <small>Pypi配置</small>
          <p>编写.pypirc配置文件如下:</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              '[distutils]\n' +
              'index-servers =' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              'pypi\n' +
              'local\n' +
              '\n' +
              '[pypi]\n' +
              'username:你的用户名\n' +
              'password:你的密码\n' +
              '\n' +
              '[local]\n' +
              'repository:' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              'username: 你的用户名\n' +
              'password: 你的密码'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          打包上传
          <small>该方式打包时指定仓库</small>
          <p>如下命令：</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'python3 -m twine upload --username admin --password folib@v587 --repository-url ' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              ' dist/* --verbose'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>Pypi 通常使用命令</small>
          <p>操作命令和通常Pypi一样使用，具体参阅：https://pypi.org/</p>

          <prism-editor
            class="my-editor height-300"
            :value="'python3 setup.py sdist bdist_wheel'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'docker'">
        <a-timeline-item color="primary">
          Ubuntu配置
          <small>针对Docker客户端版本大于 1.10.0 的用户</small>
          <p>您可以通过修改daemon配置文件/etc/docker/daemon.json来使用:</p>
          <prism-editor
            class="my-editor height-300"
            :value="dockerCode.ubuntu"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          CentOS配置
          <small>针对Docker客户端版本大于 1.10.0 的用户</small>
          <p>您可以通过修改daemon配置文件/etc/docker/daemon.json来使用:</p>
          <prism-editor
            class="my-editor height-300"
            :value="dockerCode.centos"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          MacOS配置
          <small
            >针对安装了Docker for Mac的用户，您可以参考以下配置步骤：</small
          >
          <p>
            在任务栏点击 Docker Desktop 应用图标 ->
            Perferences，在左侧导航菜单选择 Docker Engine，在右侧输入栏编辑 json
            文件。将:
          </p>
          <prism-editor
            class="my-editor height-300"
            :value="dockerCode.macos"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
          <p>
            加到"insecure-registries"的数组里，点击 Apply &
            Restart按钮，等待Docker重启
          </p>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Windows配置
          <small
            >针对安装了Docker for Windows的用户，您可以参考以下配置步骤：</small
          >
          <p>
            在系统右下角托盘图标内右键菜单选择
            Settings，打开配置窗口后左侧导航菜单选择 Docker
            Daemon。编辑窗口内的JSON串，填写下方地址：
          </p>
          <prism-editor
            class="my-editor height-300"
            :value="dockerCode.windows"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          镜像打包命名说明
          <small>请一定要看，这决定了你的镜像包能否上传：</small>
          <p>
            镜像命名规则如下：仓库访问url/存储空间/仓库名称/镜像名称:版本号，具体如下：
          </p>
          <prism-editor
            class="my-editor height-300"
            :value="
              'docker build -t ' +
              baseUrl.replace('http://', '') +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '/demo:latest .'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'nuget'">
        <a-timeline-item color="primary">
          NuGet+Mono配置
          <small>添加默认推送存储库 URL</small>
          <p>示例如下，详细请看文档</p>
          <prism-editor
            class="my-editor height-300"
            :value="
              '$ mono --runtime=v4.0 nuget.exe config -set DefaultPushSource=' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              ' -ConfigFile ./.nuget/NuGet.config'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Nuget+Visual Studio配置
          <small>以下为示例</small>
          <p>为了方便访问folib可将 -Source 选项附加到 NuGet.exe：</p>
          <prism-editor
            class="my-editor height-300"
            :value="
              'nuget <command> -Source ' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              ''
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
          <p>Visual Studio中的详细配置请看平台帮助文档</p>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'php'">
        <a-timeline-item color="primary">
          Composer认证
          <p>
            http-basic
          </p>
          <p>
            打开命令行窗口（windows用户）或控制台（Linux、Mac 用户）并执行如下命令：
          </p>
          <prism-editor class="my-editor height-300" :value="'composer config -g http-basic.' + baseUrl + ' admin folib@v587'" 
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Composer配置
          <p>
            方法一： 修改 composer 的全局配置文件（推荐方式）
          </p>
          <p>
            打开命令行窗口（windows用户）或控制台（Linux、Mac 用户）并执行如下命令：
          </p>
          <prism-editor class="my-editor height-300" :value="'composer config -g repo.packagist composer ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id" 
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
          <p>
          方法二： 修改当前项目的 composer.json 配置文件
          </p>
          <p>
            打开命令行窗口（windows用户）或控制台（Linux、Mac 用户），进入你的项目的根目录（也就是 composer.json 文件所在目录），执行如下命令：
          </p>
          <prism-editor class="my-editor height-300" :value="'composer config repo.packagist composer ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id" 
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          取消配置
          <p>#全局取消</p>
          <p>
            composer config -g --unset repos.packagist
          </p>
          <p>#项目取消</p>
          <p>
            composer config --unset repos.packagist
          </p>
          <p>
            注意：本仓库类型为:<strong>{{ folibRepository.type === 'proxy' ? '代理库' : folibRepository.type === 'group' ? '组合库' :
                '本地库'
            }}</strong>{{ folibRepository.type === 'proxy' ? '不支持上传' : folibRepository.type === 'group' ?
    '不支持上传' : '可以上传'
}}
          </p>
          <p v-if="folibRepository.type === 'hosted'">
            使用API或页面上传按钮进行上传
          </p>
        </a-timeline-item>
        <a-timeline-item color="primary">
          命令操作
          <small>composer 通常使用命令</small>
          <p>
            和通常composer一样使用，具体参阅：<a target="_blank" href="https://getcomposer.org/doc/03-cli.md">https://getcomposer.org/doc/03-cli.md</a>
          </p>

          <prism-editor class="my-editor height-300" :value="'composer init\n' +
          'composer install\n' + 
          'composer -vvv require\n' + 
          'composer clear-cache'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'cocoapods'">
        <a-timeline-item color="primary">
          通用
          <p>
            为了在Artifactory中使用CocoaPods，你需要安装"cocoapods-art"。插件。安装cocoapods-art命令如下:
          </p>
          <prism-editor class="my-editor height-300" value="gem install cocoapods-art" 
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
          <p>
            repo-art使用标准<a href="https://www.gnu.org/software/inetutils/manual/html_node/The-_002enetrc-file.html">netrc file</a>中指定的身份验证
          </p>
          <prism-editor class="my-editor height-300" 
          :highlight="highlighterHandle" :line-numbers="false" :value='"machine "+(baseUrl.endsWith("/") ? baseUrl:baseUrl+"/").replaceAll(/https?\:\/\/(.*?)(:\d+?)?\//g, "$1")+
"\rlogin <USERNAME>"+
"\rpassword <PASSWORD>"' :readonly="true">
          </prism-editor>
          <p>
            要添加一个Artifactory Specs库:
          </p>
          <prism-editor class="my-editor height-300" 
          :highlight="highlighterHandle" :line-numbers="false" :value='baseUrl+"storages/"+folibRepository.storageId+"/"+folibRepository.id' :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          部署
          <p>
            要将pod部署到Artifactory存储库中，您需要使用Artifactory的REST API或Web UI。
            例如，要使用REST API将pod部署到此存储库，请使用以下命令:
          </p>
          <prism-editor class="my-editor height-300" :value='"curl -u<USERNAME>:<PASSWORD> -XPUT "+baseUrl+"storages/"+folibRepository.storageId+"/"+folibRepository.id+"/<TARGET_FILE_PATH> -T <PATH_TO_FILE>"' 
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Podfile插件集成
          <p>
            要从您添加的 Artifactory 规范存储库中解析 Pod，您必须将以下内容添加到 Podfile 中：
          </p>
          <prism-editor class="my-editor height-300" value="plugin 'cocoapods-art', :sources => [
  'Cocoapad-Local'
]" 
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
          <p>
            然后，您可以像往常一样使用安装：
          </p>
          <prism-editor class="my-editor height-300" value="pod install" 
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline>
        <a-timeline-item color="primary">
          仓库地址
          <small>仓库使用地址</small>
          <p>
            {{ repositoryUrl }}
            <a-button type="link" slot="extra"
                @click="copy(repositoryUrl)">
                <a-icon type="copy" theme="twoTone" />
            </a-button>
          </p>
        </a-timeline-item>
      </a-timeline>
    </a-drawer>
  </div>
</template>
<script>
import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css"; // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/themes/prism-tomorrow.css";
export default {
  props: [
    "usedVisible",
    "repositoryType",
    "folibRepository",
    "ivyCode",
    "baseUrl",
    "dockerCode",
  ],
  components: {
    PrismEditor,
    // quillEditor,
  },
  data() {
    return {
      repositoryUrl: ''
    };
  },
  created() {
    if (this.baseUrl) {
      this.repositoryUrl = this.baseUrl + 'storages/' + this.folibRepository.storageId + '/' + this.folibRepository.id
      if (this.repositoryType && (this.repositoryType === 'docker' || this.repositoryType === 'conan')) {
        let baseUrlArr = this.baseUrl.split('://')
        this.repositoryUrl = baseUrlArr[1] + this.folibRepository.storageId + '/' + this.folibRepository.id
      }
    }
  },
  mounted() {},
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
    closeUsedVisibleDialog(code) {
      this.$emit("close");
    },
    copy(code) {
      var input = document.createElement("input"); // 创建input对象
      input.value = code; // 设置复制内容
      document.body.appendChild(input); // 添加临时实例
      input.select(); // 选择实例内容
      document.execCommand("Copy"); // 执行复制
      document.body.removeChild(input); // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: '复制成功'
        })
      }, 100)
    },
  },
};
</script>