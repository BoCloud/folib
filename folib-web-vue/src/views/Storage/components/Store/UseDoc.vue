<template>
  <div>
    <a-drawer
      placement="right"
      width="65%"
      :title="$t('Store.Instructions')"
      :visible="usedVisible"
      @close="closeUsedVisibleDialog"
    >
      <a-timeline v-if="repositoryType === 'maven'">
        <a-timeline-item color="primary">
          Maven{{ $t('Store.GlobalConfiguration') }}
          <small>maven settings{{ $t('Store.Configuration') }}</small>
          <p>{{ $t('Store.copyConfiguration') }}</p>

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
          POM{{ $t('Store.Configuration') }}
          <small>pom.xml{{ $t('Store.Configuration') }}</small>
          <p>
            {{ $t('Store.POMConfiguration') }}
          </p>
          <p>
            {{ $t('Store.warehouseType') }}<strong>{{
              folibRepository.type === "proxy"
                ? $t('Store.ProxyLibrary')
                : folibRepository.type === "group"
                ? $t('Store.CombinatorialLibrary')
                : $t('Store.LocalLibrary')
            }}</strong
            >{{
              folibRepository.type === "proxy"
                ? $t('Store.NotUpload')
                : folibRepository.type === "group"
                ? $t('Store.NotUpload')
                : $t('Store.SupportUpload')
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
          {{ $t('Store.CommandOperation') }}
          <small>maven {{ $t('Store.UsuallyCommand') }}</small>
          <p>
            {{ $t('Store.UsuallyUse') }}maven{{ $t('Store.specificRefer') }}https://maven.apache.org/index.html
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
          Gradle{{ $t('Store.Configuration') }}
          <small>Gradle{{ $t('Store.ConfigWarehouse') }}</small>
          <p>{{ $t('Store.gradleCode') }}</p>

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
          {{ $t('Store.CommandOperation') }}
          <small>Gradle {{ $t('Store.UsuallyCommand') }}</small>
          <p>
            {{ $t('Store.UsuallyUse') }}Gradle{{ $t('Store.specificRefer') }}https://docs.gradle.org/current/userguide/userguide.html
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
          SBT{{ $t('Store.Configuration') }}
          <small>SBT{{ $t('Store.ConfigWarehouse') }}</small>
          <p>{{ $t('Store.SBTCode') }}</p>

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
          {{ $t('Store.GlobalConfiguration') }}
          <small>SBT{{ $t('Store.GlobalConfiguration') }}</small>
          <p>
            {{ $t('Store.SBTGlobalConfig') }}
          </p>

          <prism-editor
            class="my-editor height-300"
            :value="'-Dsbt.override.build.repos=true ## ' + this.$t('Store.SBTIgnore') + '\n'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.CommandOperation') }}
          <small>SBT{{ $t('Store.CommandOperation') }}</small>
          <p>{{ $t('Store.UsuallyUse') }}SBT{{ $t('Store.specificRefer') }}https://www.scala-sbt.org/</p>

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
          ivy{{ $t('Store.Configuration') }}
          <small>ivy{{ $t('Store.ConfigWarehouse') }}</small>
          <p>
            {{ $t('Store.ivyCode') }}
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
          {{ $t('Store.CommandOperation') }}
          <small>ant-ivy{{ $t('Store.UsuallyCommand') }}</small>
          <p>
            {{ $t('Store.UsuallyUse') }}SBT{{ $t('Store.specificRefer') }}https://ant.apache.org/ivy/history/2.4.0/use/makepom.html
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
      <a-timeline v-if="repositoryType === 'npm' ">
        <a-timeline-item color="primary">
          NPM{{ $t('Store.GlobalConfiguration') }}
          <small>NPM{{ $t('Store.Configuration') }}{{ $t('Store.GlobalConfiguration') }}</small>
          <p>{{ $t('Store.ConfigMirror') }}</p>

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
              'npm config list #' + this.$t('Store.NpmConfig')
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          {{ $t('Store.EngineerAllocation') }}
          <small>{{ $t('Store.validCurrent') }}</small>
          <p>{{ $t('Store.npmrcCode') }}</p>

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
              '; `_auth` ' + this.$t('Store.IS') + ' base64 token\n' +
              '; ' + this.$t('Store.NpmAuth') + ':\n' +
              '; username=admin\n' +
              '; _password=password'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.CommandOperation') }}
          <small>NPM {{ $t('Store.UsuallyCommand') }}</small>
          <p>{{ $t('Store.UsuallyUse') }}NPM{{ $t('Store.specificRefer') }}https://npmjs.org/</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'npm install   #' + this.$t('Store.NpmInstall') + '\n' + '\n' + 'npm publish  #' + this.$t('Store.NpmPublish')
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'rpm'">
        <a-timeline-item color="primary">
          RPM{{ $t('Store.Configuration') }}
          <small>centOS yum{{ $t('Store.SourceConfiguration') }}</small>
          <p>
            {{ $t('Store.RPMOperation') }}
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
          {{ $t('Store.CommandOperation') }}
          <small>yum {{ $t('Store.UsuallyCommand') }}</small>
          <p>{{ $t('Store.ForReference') }}</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'yum clean all #' + this.$t('Store.RpmClean') +
              '\n' +
              'yum repolist #' + this.$t('Store.RpmRepoList') +
              '\n' +
              'yum install --downloadonly --downloaddir=/folib_test/mysql mysql'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'helm'">
        <a-timeline-item color="primary">
          Helm{{ $t('Store.Configuration') }}
          <p>{{ $t('Store.folibOperation') }}</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'helm  registry  login  ' +
              baseUrl +'storages/'+
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              '\n' +
              'helm  repo  add   ' +
              folibRepository.id +
              '   ' +
              baseUrl +'storages/'+
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
          {{ $t('Store.ChartPackage') }}
          <p>{{ $t('Store.HelmPlugin') }}</p>
          <prism-editor
            class="my-editor height-300"
            :value="
              '1.   https://github.com/chartmuseum/helm-push/releases ' + this.$t('Store.HelmCMPush') +
              '\n' +
              '2.   ' + this.$t('Store.HelmDecompression') +
              '\n' +
              '\n' +
              '\n' +
              '\n' +
              'helm-cm-push ' + this.$t('Store.HelmUploadCmd') +
              '\n' +
              '\n' +
              '1. ' + this.$t('Store.HelmUploadCmdBin') +
              '\n' +
              '\n' +
              '2.  ' + this.$t('Store.HelmUploadExecute') +
              '\n' +
              this.$t('Store.HelmUploadExample') +
              folibRepository.id +
              '\n' +
              '\n' +
              './helm-cm-push  /app/fluentd-4.5.2.tgz  ' +
              folibRepository.id +
              '\n' +
              '\n' +
              this.$t('Store.HelmUploadParamsExplain') +
              '\n'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          helm {{ $t('Store.UsuallyCommand') }}
          <p>{{ $t('Store.HelmCommand') }} https://helm.sh/zh/docs/intro/using_helm/</p>
          <prism-editor
            class="my-editor height-300"
            :value="
              'helm repo update  #' + this.$t('Store.HelmRepoUpdate') +
              '\n' +
              '\n' +
              'helm search repo mysql     #' + this.$t('Store.HelmRepoSearch') +
              '\n' +
              '\n' +
              'helm pull  ' +
              folibRepository.id +
              '/mysql       #' + this.$t('Store.HelmPull') +
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
          Conan {{ $t('Store.Configuration') }}
          <p></p>
          <prism-editor
            class="my-editor height-300"
            :value="
              '1. conan ' + this.$t('Store.Client') + '1.X\n' +
              '#' + this.$t('Store.ConanRepositoryAdd') + '\n' +
              'conan remote add ' +
              folibRepository.id +
              ' ' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              ' false\n' +
              '#' + this.$t('Store.ConanLogin') + ' \n' +
              'conan user -p [password] -r ' + folibRepository.id +' [username] \n\n' +
              '2. conan '+ this.$t('Store.Client') + '2.X\n' +
              '#' + this.$t('Store.ConanRepositoryAdd') + '\n' +
              'conan remote add ' +
              folibRepository.id +
              ' ' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              ' --insecure -f\n ' +
              '#' + this.$t('Store.ConanLogin') + ' \n' +
              'conan remote login -p [password] ' + folibRepository.id + ' [username]'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Conan{{ $t('Store.Package') }} {{ $t('Store.ConanOperation') }}
          <p></p>
          <prism-editor
            class="my-editor height-300"
            :value="
              '1. conan '+ this.$t('Store.Client') + '1.X\n' +
              '#' + this.$t('Store.ConanSearch') +
              '\n' +
              'conan search ' +
              '\n' +
              '#' + this.$t('Store.ConanCreate') +
              '\n' +
              'conan create . -r ' +
              folibRepository.id + '\n' +
              '#' + this.$t('Store.ConanUpload') + '\n' +
              'conan upload example/1.0 -r ' +
              folibRepository.id +
              ' -c --all --force' +
              '\n\n' +
              '2. conan '+ this.$t('Store.Client') + '2.X\n' +
              '#' + this.$t('Store.ConanSearch') +
              '\n' +
              'conan list [pattern]' +
              '\n' +
              '#' + this.$t('Store.ConanCreate') +
              '\n' +
              'conan create . -r ' +
              folibRepository.id + '\n' +
              '#' + this.$t('Store.ConanUpload') + '\n' +
              'conan upload example/1.0 -r ' +
              folibRepository.id +
              ' -c --force'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>

        <a-timeline-item color="primary">
          Conan {{ $t('Store.UsuallyCommand') }}
          <p></p>
          <prism-editor
            class="my-editor height-300"
            :value="'#' + this.$t('Store.ConanRemoteList') + '\n conan remote list'"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
          <p>
            {{ $t('Store.HelmCommand') }}
            <a href="https://docs.conan.io/en/latest/reference/commands.html" target="_blank">https://docs.conan.io/en/latest/reference/commands.html</a>
          </p>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'yarn'">
        <a-timeline-item color="primary">
          Yarn{{ $t('Store.Configuration') }}
          <small>Yarn{{ $t('Store.Configuration') }}{{ $t('Store.GlobalConfiguration') }}</small>
          <p>{{ $t('Store.YarnConfig') }}</p>

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
              'yarn config get registry #' + this.$t('Store.NpmConfig')
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.EngineerAllocation') }}
          <small>{{ $t('Store.validCurrent') }}</small>
          <p>{{ $t('Store.npmrcCode') }}</p>

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
              '; `_auth` ' + this.$t('Store.IS') + ' base64 token\n' +
              '; ' + this.$t('Store.NpmAuth') + ':\n' +
              '; username=admin\n' +
              '; _password=password'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.CommandOperation') }}
          <small>Yarn {{ $t('Store.UsuallyCommand') }}</small>
          <p>{{ $t('Store.UsuallyUse') }}Yarn{{ $t('Store.specificRefer') }}https://npmjs.org/</p>

          <prism-editor
            class="my-editor height-300"
            :value="
              'yarn install   #' + this.$t('Store.NpmInstall') + '\n' + '\n' + 'yarn publish  #' + this.$t('Store.NpmPublish')
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'pypi'">
        <a-timeline-item color="primary" v-if="folibRepository.type !== 'hosted'">
          Pypi{{ $t('Store.Configuration') }}
          <small>Pypi{{ $t('Store.Configuration') }}</small>
          <prism-editor
            class="my-editor height-300"
            :value=" 'pip config set global.index-url ' + baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\npip config set install.trusted-host ' + baseUrl.replace('http://', '').replace('https://','').replace('/','')
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary" v-if="folibRepository.type === 'hosted'">
          {{ $t('Store.PackageUpload') }}
          <small>{{ $t('Store.DesignatedRepository') }}</small>
          <p>{{ $t('Store.PypiConfig') }}</p>

          <prism-editor
            class="my-editor height-300"
            :value=" '[distutils]\n' +
              'index-servers =' +
              '\n    ' + folibRepository.id +
              '\n' +
              '['+ folibRepository.id +']\n' +
              'repository:' +
              baseUrl +
              'storages/' +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '\n' +
              'username:[username]\n' +
              'password:[password]\n'
            "
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
          <p>{{ $t('Store.followingCommand') }}</p>
          <prism-editor
            class="my-editor height-300"
            :value="'python3 -m twine upload --repository ' +
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
          {{ $t('Store.CommandOperation') }}
          <small>Pypi {{ $t('Store.UsuallyCommand') }}</small>
          <p>{{ $t('Store.UsuallyUse') }}Pypi{{ $t('Store.specificRefer') }}https://pypi.org/</p>

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
          Ubuntu{{ $t('Store.Configuration') }}
          <small>{{ $t('Store.DockerVersions') }}</small>
          <p>{{ $t('Store.modifyDaemon') }}</p>
          <prism-editor
            class="my-editor height-300"
            :value="dockerCode.ubuntu"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          CentOS{{ $t('Store.Configuration') }}
          <small>{{ $t('Store.DockerVersions') }}</small>
          <p>{{ $t('Store.modifyDaemon') }}</p>
          <prism-editor
            class="my-editor height-300"
            :value="dockerCode.centos"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          MacOS{{ $t('Store.Configuration') }}
          <small
            >{{ $t('Store.MacDocker') }}</small
          >
          <p>
            {{ $t('Store.clickIcon') }}
            {{ $t('Store.selectLeft') }}
            {{ $t('Store.will') }}
          </p>
          <prism-editor
            class="my-editor height-300"
            :value="dockerCode.macos"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
          <p>
            {{ $t('Store.clickButton') }}
            {{ $t('Store.waitDocker') }}
          </p>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Windows{{ $t('Store.Configuration') }}
          <small
            >{{ $t('Store.WindowsDocker') }})
          </small>
          <p>
            {{ $t('Store.RightClickMenu') }}
            {{ $t('Store.LeftClickMenu') }}
            {{ $t('Store.EditJSON') }}
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
          {{ $t('Store.ImagePackage') }}
          <small>{{ $t('Store.ImagePackageTips') }}</small>
          <p>
            {{ $t('Store.ImageNamingRules') }}
          </p>
          <prism-editor
            class="my-editor height-300"
            :value="
              'docker build -t ' +
              baseUrl.replace('http://', '').replace('https://','') +
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

        <a-timeline-item color="primary">
          {{ $t('Store.ImageBuildx') }}
          <small>{{ $t('Store.ImageBuildxTips') }}</small>
          <p>
            {{ $t('Store.ImageBuildxRules') }}
          </p>
          <prism-editor
              class="my-editor height-300"
              :value="
              'docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64/v8 -t ' +
              baseUrl.replace('http://', '').replace('https://','') +
              folibRepository.storageId +
              '/' +
              folibRepository.id +
              '/demo:latest . --push'
            "
              :highlight="highlighterHandle"
              :line-numbers="false"
              :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'nuget'">
        <a-timeline-item color="primary">
          NuGet+Mono{{ $t('Store.Configuration') }}
          <small>{{ $t('Store.URLRepository') }} URL</small>
          <p>{{ $t('Store.SeeDocumentation') }}</p>
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
          Nuget+Visual Studio{{ $t('Store.Configuration') }}
          <small>{{ $t('Store.example') }}</small>
          <p>{{ $t('Store.accessFolib') }}</p>
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
          <p>{{ $t('Store.VSConfig') }}</p>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'php'">
        <a-timeline-item color="primary">
          Composer{{ $t('Store.Authentication') }}
          <p>
            http-basic
          </p>
          <p>
            {{ $t('Store.ExecuteCommand') }}
          </p>
          <prism-editor class="my-editor height-300" :value="'composer config -g http-basic.' + baseUrl + ' admin folib@v587'"
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Composer {{ $t('Store.Configuration') }}
          <p>
            {{ $t('Store.Method1') }}
          </p>
          <p>
            {{ $t('Store.ExecuteCommand') }}
          </p>
          <prism-editor class="my-editor height-300" :value="'composer config -g repo.packagist composer ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id"
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
          <p>
            {{ $t('Store.Method2') }}
          </p>
          <p>
            {{ $t('Store.ExecuteCommand') }}
          </p>
          <prism-editor class="my-editor height-300" :value="'composer config repo.packagist composer ' + baseUrl + 'storages/' + folibRepository.storageId + '/' + folibRepository.id"
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.Unconfigure') }}
          <p>#{{ $t('Store.GlobalCancel') }}</p>
          <p>
            composer config -g --unset repos.packagist
          </p>
          <p>#{{ $t('Store.ProjectCancel') }}</p>
          <p>
            composer config --unset repos.packagist
          </p>
          <p>
            {{ $t('Store.warehouseType') }}<strong>{{
              folibRepository.type === "proxy"
                ? $t('Store.ProxyLibrary')
                : folibRepository.type === "group"
                ? $t('Store.CombinatorialLibrary')
                : $t('Store.LocalLibrary')
            }}</strong
          >{{
              folibRepository.type === "proxy"
                ? $t('Store.NotUpload')
                : folibRepository.type === "group"
                ? $t('Store.NotUpload')
                : $t('Store.SupportUpload')
            }}</p>
          <p v-if="folibRepository.type === 'hosted'">
            {{ $t('Store.useAPI') }}
          </p>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.CommandOperation') }}
          <small>composer {{ $t('Store.UsuallyCommand') }}</small>
          <p>
            {{ $t('Store.UsuallyUse') }}composer{{ $t('Store.specificRefer') }} <a target="_blank" href="https://getcomposer.org/doc/03-cli.md">https://getcomposer.org/doc/03-cli.md</a>
          </p>

          <prism-editor class="my-editor height-300" :value="'composer init\n' +
          'composer install\n' +
          'composer -vvv require\n' +
          'composer clear-cache'" :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'cocoapods'">
        <a-timeline-item color="primary">
          {{ $t('Store.GeneralPurpose') }}
          <p>
            {{ $t('Store.useCocoapods') }}
          </p>
          <prism-editor class="my-editor height-300" value="gem install cocoapods-art"
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
          <p>
            {{ $t('Store.UseStandards') }}<a href="https://www.gnu.org/software/inetutils/manual/html_node/The-_002enetrc-file.html">netrc file</a>{{ $t('Store.SpecifyAuthentication') }}
          </p>
          <prism-editor class="my-editor height-300"
          :highlight="highlighterHandle" :line-numbers="false" :value='"machine "+(baseUrl.endsWith("/") ? baseUrl:baseUrl+"/").replaceAll(/https?\:\/\/(.*?)(:\d+?)?\//g, "$1")+
"\rlogin <USERNAME>"+
"\rpassword <PASSWORD>"' :readonly="true">
          </prism-editor>
          <p>
            {{ $t('Store.addLibrary') }}
          </p>
          <prism-editor class="my-editor height-300"
          :highlight="highlighterHandle" :line-numbers="false" :value='"pod repo-art add "+folibRepository.id+" \""+baseUrl+"storages/"+folibRepository.storageId+"/"+folibRepository.id + "\""' :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.Deployment') }}
          <p>
            {{ $t('Store.podDeployment') }}
            {{ $t('Store.podDeployExample') }}
          </p>
          <prism-editor class="my-editor height-300" :value='"curl -u<USERNAME>:<PASSWORD> -XPUT "+baseUrl+"storages/"+folibRepository.storageId+"/"+folibRepository.id+"/<TARGET_FILE_PATH> -T <PATH_TO_FILE>"'
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          Podfile{{ $t('Store.PlugIntegration') }}
          <p>
            {{ $t('Store.parsePod') }}
          </p>
          <prism-editor class="my-editor height-300" :value="'plugin  \'cocoapods-art\', :sources => [\r'+
'  \'' + folibRepository.id + '\'\r' +
']'"
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
          <p>
            {{ $t('Store.use') }}
          </p>
          <prism-editor class="my-editor height-300" value="pod install"
          :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'go'">
        <a-timeline-item color="primary" v-if="folibRepository.type !== 'hosted'">
          {{ $t('Store.GoProxyConfig') }}
          <p>
            {{ $t('Store.useGo') }}
          </p>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" :value='"go env -w GOPROXY="+repositoryUrl' :readonly="true">
          </prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary" v-if="folibRepository.type !== 'hosted'">
            {{ $t('Store.CommandOperation') }}
            <small>Go {{ $t('Store.UsuallyCommand') }}</small>
            <p>{{ $t('Store.UsuallyUse') }}Go{{ $t('Store.specificRefer') }}https://go.dev</p>

            <prism-editor
                    class="my-editor height-300"
                    :value="
          '#' +   this.$t('Store.GoGet')+ '\n go get   \n' +
          '#' +   this.$t('Store.GoClean')+ '\n go clean -modcache   \n' + 
          '#' +   this.$t('Store.GoDownload')+'\n go mod download'
        "
                    :highlight="highlighterHandle"
                    :line-numbers="false"
                    :readonly="true"
            ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary" v-if="folibRepository.type === 'hosted'">
          {{ $t('Store.GoUploadPreparation') }}
          <p>
            {{ $t('Store.GoUploadCreateFile') }}
          </p>
          <p>
            <strong>1、*.info</strong> <br>
            {{ $t('Store.GoFilenameFormat') }}：<strong>{{ $t('Store.Version') }}.info</strong> <br>
            *.info {{ $t('Store.GoInfoFormat') }} <br>
            <strong>{"Version":"{{ $t('Store.Version') }}","Time","{{ $t('Store.GoInfoTime') }}"}</strong> <br>
            {{ $t('Store.Example') }}：<br>
            {{ $t('Store.Version') }}：<strong>v1.1</strong> <br>
            {{ $t('Store.Filename') }}：<strong>v1.1.info</strong> <br>
            {{ $t('Store.FileContent') }}：<strong>{"Version":"v1.1","Time","2024-01-08T13:53:26Z"}</strong> <br><br>
            <strong>2、*.mod</strong> <br>
            {{ $t('Store.GoFilenameFormat') }}：<strong>{{ $t('Store.Version') }}.mod</strong> <br>
            <strong>{{ $t('Store.GoModFormat') }}</strong> <br>
            {{ $t('Store.Example') }}：<br>
            {{ $t('Store.Filename') }}：<strong>v1.1.mod</strong> <br>
            {{ $t('Store.FileContent') }}：<prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" value='module example.com/example/hello-world
go 1.20' :readonly="true">
            </prism-editor> <br><br>
            <strong>3、*.zip</strong> <br>
            {{ $t('Store.GoFilenameFormat') }}：<strong>{{ $t('Store.Version') }}.zip</strong> <br>
            <strong>{{ $t('Store.GoZipFormat') }}</strong> <br>
            {{ $t('Store.Example') }}： <br>
            {{ $t('Store.Filename') }}：<strong>v1.1.zip</strong> <br>
            {{ $t('Store.FileContent') }}： {{ $t('Store.GoZipConentTip1') }}<br>
            {{ $t('Store.GoZipConentTip2') }}<br>
          </p>
        </a-timeline-item>
        <a-timeline-item color="primary" v-if="folibRepository.type === 'hosted'">
          {{ $t('Store.GoUploadCommand')}}
          <p>
            {{ $t('Store.GoUploadUseFolib')}}
          </p>
          <p>
            1、{{ $t('Store.Login')}}
          </p>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" value='folib server login -H [host-url]' :readonly="true"></prism-editor>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" :value="'#' + this.$t('Store.Example') + ' \nfolib server login -H ' + this.baseDomain" :readonly="true"></prism-editor>

          <p>
            2、{{ $t('Store.Upload')}}
          </p>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" value='folib artifact upload -S [storageID] -r [repositoryID] -T [TargetPath] -f [filePath]' :readonly="true"></prism-editor>
          <p>
            {{ $t('Store.ParameterExplanation') }}： <br>
            storageID：{{ $t('Store.StorageName') }} <br>
            repositoryID：{{ $t('Store.RepositoryName') }} <br>
            TargetPath：{{ $t('Store.TargetPath') }} <br>
            filePath：{{ $t('Store.ArtifactSelf') }}
          </p>
          *.info、*.mod、*.zip {{ $t('Store.ArtifactThree') }}
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" :value="'#' + this.$t('Store.Example') + ' \n folib artifact upload -S ' + folibRepository.storageId + ' -r ' + folibRepository.id + ' -T example.com/example/hello-world/@v/v1.1.info -f v1.1.info \n folib artifact upload -S ' + folibRepository.storageId + ' -r ' + folibRepository.id + ' -T example.com/example/hello-world/@v/v1.1.mod -f v1.1.mod \n folib artifact upload -S ' + folibRepository.storageId + ' -r ' + folibRepository.id + ' -T example.com/example/hello-world/@v/v1.1.zip -f v1.1.zip'" :readonly="true"></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'ohpm' ">
            <a-timeline-item color="primary">
                OHPM{{ $t('Store.GlobalConfiguration') }}
                <small>OHPM{{ $t('Store.Configuration') }}{{ $t('Store.GlobalConfiguration') }}</small>
                <p>{{ $t('Store.OhpmConfigMirror') }}</p>

                <prism-editor
                        class="my-editor height-300"
                        :value="
              '#'+this.$t('Store.SshKeygen')+'\n'+
              'ssh-keygen -m PEM -t RSA -b 4096 -f {your_key_path}  \n'+
              '#'+this.$t('Store.PrivateKeyPath')+'\n'+
              'ohpm config set key_path {your_key_path}  \n'+
              '#'+this.$t('Store.SetPublishingId')+'\n'+
              'ohpm config set publish_id {your_publish_id}  \n'+
              '#'+this.$t('Store.PublishIdTip')+'\n'+
              '#'+this.$t('Store.SetPublishingIdPlus')+'\n'+
              ''+baseUrl.replace('http:','').replace('https:','')+'storages/'+folibRepository.storageId+'/'+folibRepository.id+'/:_auth={token}\n'+
              '#'+this.$t('Store.SetRepository')+'\n'+
              'ohpm config set registry '+baseUrl+'storages/'+folibRepository.storageId+'/'+folibRepository.id+'\n'+
              '#'+this.$t('Store.SetPublishingRepository')+'\n'+
              'ohpm config set publish_registry '+baseUrl+'storages/'+folibRepository.storageId+'/'+folibRepository.id+'\n'+
              '#'+this.$t('Store.SslCheck')+'\n'+
              'ohpm config set strict_ssl false \n'+
              '\n' +
              '#' +this.$t('Store.OhpmConfig')+'\n'+
              'ohpm config list -j '
            "
                        :highlight="highlighterHandle"
                        :line-numbers="false"
                        :readonly="true"
                >
                </prism-editor>
            </a-timeline-item>
            <a-timeline-item color="primary">
                {{ $t('Store.CommandOperation') }}
                <small>OHPM {{ $t('Store.UsuallyCommand') }}</small>
                <p>{{ $t('Store.UsuallyUse') }}OHPM{{ $t('Store.specificRefer') }}https://ohpm.openharmony.cn/</p>

                <prism-editor
                        class="my-editor height-300"
                        :value="
              '#'+   this.$t('Store.NpmInstall')+ '\n ohpm install   \n\n'
              + '#' + this.$t('Store.NpmPublish')+'\n'+ 'ohpm publish'
            "
                        :highlight="highlighterHandle"
                        :line-numbers="false"
                        :readonly="true"
                ></prism-editor>
            </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'gitlfs' ">
          <a-timeline-item color="primary">
              GitLfs{{ $t('Store.LfsInitialization') }}
              <small>GitLfs{{ $t('Store.Configuration') }}{{ $t('Store.LfsInitialization') }}</small>

              <prism-editor
                      class="my-editor height-300"
                      :value="
            '\n#'+this.$t('Store.LfsInitialization')+'\n'+
            'git  lfs  install \n'+
            '#'+this.$t('Store.LfsAddFile')+'\n'+
            'git  lfs  track  *.psd  \n'+
            '#'+this.$t('Store.SetLfs')+'\n'+
            'git  config  lfs.url  '+baseUrl+'storages/'+folibRepository.storageId+'/'+folibRepository.id+'\n'+
            '\n' +
            '#' +this.$t('Store.ShowLfs')+'\n'+
            'git  config  --list '
          "
                      :highlight="highlighterHandle"
                      :line-numbers="false"
                      :readonly="true"
              >
              </prism-editor>
          </a-timeline-item>
          <a-timeline-item color="primary">
              {{ $t('Store.CommandOperation') }}
              <small>GitLfs {{ $t('Store.UsuallyCommand') }}</small>
              <p>{{ $t('Store.UsuallyUse') }}GitLfs{{ $t('Store.specificRefer') }}https://git-lfs.com</p>

              <prism-editor
                      class="my-editor height-300"
                      :value="
            '#'+ this.$t('Store.LfsClone')+ '\n git  clone  --config  lfs.url='+baseUrl+'storages/'+folibRepository.storageId+'/'+folibRepository.id+'  [repository-url]  \n'+
            '#' + this.$t('Store.LfsPull')+'\n'+ 'git  lfs  pull \n'+
            '#' + this.$t('Store.LfsPush')+'\n'+ 'git  lfs  push  origin  master\n'+
            '#' + this.$t('Store.LfsAddLock')+'\n'+ 'git  lfs  lock  [' + this.$t('Store.LfsFilename') + '] \n'+
            '#' + this.$t('Store.LfsGetLocks')+'\n'+ 'git lfs locks\n' +
            '#' + this.$t('Store.LfsUnlock')+'\n'+ 'git lfs unlock [' + this.$t('Store.LfsFilename') + ']\n'
          "
                      :highlight="highlighterHandle"
                      :line-numbers="false"
                      :readonly="true"
              ></prism-editor>
          </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'huggingface' ">
          <a-timeline-item color="primary">
              {{ $t('Store.HuggingFaceMLConfigure') }}
              <p></p>
              <prism-editor
                      class="my-editor height-300"
                      :value="
            'export HF_HUB_ETAG_TIMEOUT=1500000000 \n'+
            'export HF_ENDPOINT='+baseUrl+'storages/'+folibRepository.storageId+'/'+folibRepository.id+'\n'"
                      :highlight="highlighterHandle"
                      :line-numbers="false"
                      :readonly="true"
              >
              </prism-editor>
              <small>{{$t('Store.HuggingFaceMLConfigureInfo2')}}</small>
              <small>{{$t('Store.HuggingFaceMLConfigureToken')}}</small>
              <p></p>
              <prism-editor
                      class="my-editor height-300"
                      :value="
            'export HF_TOKEN='+this.userToken"
                      :highlight="highlighterHandle"
                      :line-numbers="false"
                      :readonly="true"
              >
              </prism-editor>
          </a-timeline-item>
          <a-timeline-item color="primary">
              {{ $t('Store.HuggingFaceMLUpload') }}
              <p></p>
              <prism-editor
                      class="my-editor height-300"
                      :value="
                        'from huggingface_hub import HfApi \n'+
                        'api = HfApi()\n'+
                        'api.upload_folder(\n'+
                        '\tfolder_path='+'{folder_name}, \t\t# folder to upload location on the FS \n'+
                        '\trepo_id='+'{model_name}'+', \t\t# defines the name under which model will be saved in the local repo. (models--${model_name})\n'+
                        '\trevision='+'{model_revision}'+', \t\t# represents git revision under which files are stored (main by default) (snapshots/${revision}/...files)\n'+
                        '\trepo_type='+'model'+'\n'+')'
                        "
                      :highlight="highlighterHandle"
                      :line-numbers="false"
                      :readonly="true"
              ></prism-editor>
          </a-timeline-item>

          <a-timeline-item color="primary">
              {{ $t('Store.HuggingFaceMLDownload') }}
              <p></p>
              <prism-editor
                      class="my-editor height-300"
                      :value="
              'from huggingface_hub import snapshot_download \n'+
              'snapshot_download(\n'+
              '\trepo_id='+'{'+'model_name'+'}'+', revision='+'{'+'model_revision'+'}'+', etag_timeout=1500000000 \n)'
              "
                      :highlight="highlighterHandle"
                      :line-numbers="false"
                      :readonly="true"
              ></prism-editor>
              <p></p>
              <small>{{ $t('Store.HuggingFaceMLDownloadInfo2') }}<a href="https://hugging-face.cn/docs/huggingface_hub/quick-start">{{ $t('Store.HuggingFaceMLDownloadInfo3') }}</a></small>

          </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="repositoryType === 'pub'">
        <a-timeline-item color="primary" v-if="folibRepository.type !== 'hosted'">
          Pub {{ $t('Store.GlobalConfiguration') }}
          <small>Pub{{ $t('Store.Configuration') }}{{ $t('Store.GlobalConfiguration') }}</small>
          <p>{{ $t('Store.PubConfigMirror') }}</p>
          <small><a @click="getUserSecurityToken">{{  $t('Store.GetToken') }}</a></small>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" :value='"#" + this.$t("Store.AddToken") + "\ndart pub token add "  + "\"" + repositoryUrl + "\"\n#" + this.$t("Store.ExportPub") + "\nexport PUB_HOSTED_URL=" + "\"" + repositoryUrl + "\""' :readonly="true">
          </prism-editor>
          <br>
        </a-timeline-item>
        <a-timeline-item color="primary" v-if="folibRepository.type === 'hosted'">
          Pub {{ $t('Store.PubPublishConfig') }}
          <p>{{ $t('Store.PubPublishConfigMirror') }}</p>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" :value='"publish_to: " + repositoryUrl' :readonly="true">
          </prism-editor>
          <p>{{ $t('Store.PubPublishTokenConfig') }}</p>
          <small><a @click="getUserSecurityToken">获取token</a></small>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" :value='"dart pub token add "  + "\"" + repositoryUrl + "\""' :readonly="true">
          </prism-editor>
          <p>{{ $t('Store.PubPublish') }}</p>
          <prism-editor class="my-editor height-300"
                        :highlight="highlighterHandle" :line-numbers="false" :value='"dart pub publish"' :readonly="true">
          </prism-editor>
          <br>
        </a-timeline-item>
        <a-timeline-item color="primary">
            {{ $t('Store.CommandOperation') }}
            <small>Pub {{ $t('Store.UsuallyCommand') }}</small>
            <p>{{ $t('Store.UsuallyUse') }}Pub{{ $t('Store.specificRefer') }}https://dart.cn/tools/dart-tool</p>

            <prism-editor
                    class="my-editor height-300"
                    :value="
          '#' +   this.$t('Store.PubAdd')+ '\n dart pub add   \n' +
          '#' +   this.$t('Store.PubCacheClean')+ '\n dart pub cache clean   \n' + 
          '#' +   this.$t('Store.PubGet')+'\n dart pub get   \n' + 
          '#' +   this.$t('Store.PubPublish')+ '\n dart pub publish   \n'
        "
                    :highlight="highlighterHandle"
                    :line-numbers="false"
                    :readonly="true"
            ></prism-editor>
        </a-timeline-item>
      </a-timeline>
      <a-timeline v-if="folibRepository.layout === 'debian'">
        <a-timeline-item color="primary">
          Debian {{ $t('Store.GlobalConfiguration') }}
          <small>Debian{{ $t('Store.Configuration') }}{{ $t('Store.GlobalConfiguration') }}</small>
          <p>{{ $t('Store.DebianGlobalConfiguration') }}</p>
          <prism-editor
            class="my-editor height-300"
            :value="debianConfiguration"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
          <p>{{ $t('Store.DebianPermissionConfiguration') }}</p>
          <prism-editor
            class="my-editor height-300"
            :value="debianPermissonConfiguration"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
        <a-timeline-item color="primary">
          {{ $t('Store.CommandOperation') }}
          <small>{{ $t('Store.DebianCommandOperation') }}</small>
          <prism-editor
            class="my-editor height-300"
            :value="debianCommand"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>
        </a-timeline-item>
      </a-timeline>
        <!--cargo 使用说明 -->
      <a-timeline v-if="folibRepository.layout === 'cargo'">
          <a-timeline-item color="primary">
              Cargo {{ $t('Store.GlobalConfiguration') }}
              <p>{{ $t('Store.CargoGlobalConfiguration') }}</p>
              <prism-editor
                  class="my-editor height-300"
                  :value="cargoConfiguration"
                  :highlight="highlighterHandle"
                  :line-numbers="false"
                  :readonly="true"
              >
              </prism-editor>
              <p>{{ $t('Store.CargoGlobalConfigurationToken') }}</p>
              <prism-editor
                  class="my-editor height-300"
                  :value="cargoConfigurationToken"
                  :highlight="highlighterHandle"
                  :line-numbers="false"
                  :readonly="true"
              ></prism-editor>
          </a-timeline-item>

          <a-timeline-item color="primary">
              Cargo {{ $t('Store.CargoDeploy') }}
              <p>{{ $t('Store.CargoDeployConfig') }}</p>
              <prism-editor
                  class="my-editor height-300"
                  :value="cargoDeploy"
                  :highlight="highlighterHandle"
                  :line-numbers="false"
                  :readonly="true"
              >
              </prism-editor>
          </a-timeline-item>
          <a-timeline-item color="primary">
              Cargo {{ $t('Store.CargoInstall') }}
              <p>{{ $t('Store.CargoInstallConfig') }}</p>
              <prism-editor
                  class="my-editor height-300"
                  :value="cargoInstall"
                  :highlight="highlighterHandle"
                  :line-numbers="false"
                  :readonly="true"
              >
              </prism-editor>
          </a-timeline-item>

      </a-timeline>

      <a-timeline>
        <a-timeline-item color="primary">
          {{ $t('Store.WarehouseAddress') }}
          <small>{{ $t('Store.WarehouseUseAddress') }}</small>
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
import {getUserToken} from "@/api/users";
import { generateUserSecurityToken } from "@/api/users";
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
      repositoryUrl: '',
      baseDomain: '',
      userToken:''
    };
  },
  created() {
      console.log("repositoryType",this.repositoryType )
      console.log("folibRepository",this.folibRepository )
    if (this.baseUrl) {
      this.repositoryUrl = this.baseUrl + 'storages/' + this.folibRepository.storageId + '/' + this.folibRepository.id
      if (this.repositoryType && this.repositoryType === 'docker') {
        let baseUrlArr = this.baseUrl.split('://')
        this.repositoryUrl = baseUrlArr[1] + this.folibRepository.storageId + '/' + this.folibRepository.id
      }
        this.baseDomain = this.baseUrl.endsWith('/') ? this.baseUrl.slice(0,this.baseUrl.length - 1):this.baseUrl
    }
  },
  mounted() {
      this.huggingfaceInit();
  },
  computed: {
    debianConfiguration() {
      return  `sudo sh -c " echo  'deb ${this.baseUrl}storages/${this.folibRepository.storageId}/${this.folibRepository.id} <DISTRIBUTION> <COMPONENT> ' >> /etc/apt/sources.list "`
    },
    debianPermissonConfiguration() {
      const name = this.$store.getters.name;
      const url = this.baseUrl.replace('http://','').replace('https://','');
      const protocol = this.baseUrl.startsWith('http://') ? 'http://' : 'https://';
      return  `sudo sh -c " echo  'deb  ${protocol}${name}:<PASSWORD>@${url}storages/${this.folibRepository.storageId}/${this.folibRepository.id} <DISTRIBUTION> <COMPONENT> ' >> /etc/apt/sources.list "`
    },
    debianCommand(){
      return ` apt update --allow-insecure-repositories \n apt-get install <DEBIAN_PACKAGE_NAME>`
    },
      cargoConfiguration() {
          return `[registry]\ndefault = "folib"\n\n[registries.folib]\nindex = "sparse+${this.baseDomain}storages/${this.folibRepository.storageId}/${this.folibRepository.id}/index/"`
      },
      cargoConfigurationToken() {
        return `[registry.folib]\ntoken = "Bearer <TOKEN>"\n#token = "Basic <BASE64>"`
      },
      cargoDeploy(){
        return `cargo login "Bearer <TOKEN>"\ncargo publish --registry folib`
      },
      cargoInstall(){
          return `cargo login "Bearer <TOKEN>"\ncargo install <PACKAGE_NAME>`
      },
  },
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
    closeUsedVisibleDialog(code) {
      this.$emit("close")
    },
    copy(code) {
      var input = document.createElement("input"); // $t('Store.创建')input$t('Store.对象')
      input.value = code; // $t('Store.设置复制内容')
      document.body.appendChild(input); // $t('Store.添加临时实例')
      input.select(); // $t('Store.选择实例内容')
      document.execCommand("Copy"); // $t('Store.执行复制')
      document.body.removeChild(input); // $t('Store.删除临时实例')
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: this.$t('Store.CopySuccess')
        })
      }, 100)
    },
     huggingfaceInit(){
          if(this.folibRepository.layout === 'HuggingFace' ){
              generateUserSecurityToken({expireSeconds: 1892160000}).then((res) => {
                  this.userToken = res;
              })
          }
      },
    getUserSecurityToken() {
      generateUserSecurityToken({expireSeconds: 1892160000}).then((res) => {
        this.copy(res)
      }).finally(() => {})
    }
  },
};
</script>
