<template>
  <div class="artifact-search-base-data">
    <a-tabs default-active-key="1" @change="artifactTabChange">
      <a-tab-pane key="1" :tab="$t('Store.BasicInformation')">
        <a-descriptions
          v-if="repositoryType !== 'Docker' && artifact"
          title=""
          :column="1"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item :label="$t('Store.OwningSpace')">
            {{ artifact.storageId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.OwnedWarehouse')">
            {{ artifact.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.Name')">
            {{ artifactName }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.ThePath')">
            {{ artifactPath }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.FileSize')">
            {{ fileSizeConver(artifact.sizeInBytes) }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.ModifyTheTime')">
            {{ currentArtifact.lastModified }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.LastUsedTime')">
            {{ currentArtifact.lastUsedTime }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.DownloadTimes')">
            {{ artifact.downloadCount }}
          </a-descriptions-item>
          <template v-if="artifact && artifact.checksums">
            <a-descriptions-item :label="key" v-for="(value, key, index) in artifact.checksums" :key="index" span="2">
              {{ value }}
            </a-descriptions-item>
          </template>
        </a-descriptions>
        <div v-if="currentArtifact && currentArtifact.manifest && currentArtifact.manifest.manifests">
          <a-tag class="mb-10" :color="index === selectedTag? selectedColor : ''" v-for="(item, index) in currentArtifact.manifest.manifests" :key="index" @click="clickTag(item, index)">
            <a> {{ item.platform.os + '/' + item.platform.architecture + (item.platform.variant? '/' + item.platform.variant : '') }} </a>
          </a-tag>
        </div>
        <a-descriptions
          v-if="repositoryType === 'Docker' && artifact"
          title=""
          :column="1"
        >
          <a-descriptions-item :label="$t('Store.OwningSpace')">
            {{ artifact.storageId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.OwningSpace')">
            {{ artifact.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item v-if="artifact" :label="$t('Store.ImageName')">
            {{ artifact.artifactCoordinates.imageName }}
          </a-descriptions-item>
          <a-descriptions-item :label="artifact ? $t('Store.VersionNumber') : $t('Store.Name')">
            {{ artifactName }}
          </a-descriptions-item>
          <a-descriptions-item v-if="artifact" :label="$t('Store.FileSize')">
            {{ fileSizeConver(currentArtifact.size) }}
          </a-descriptions-item>
          <a-descriptions-item v-if="artifact" label="SHA-256">
            {{ currentArtifact.sha256 }}
          </a-descriptions-item>
          <a-descriptions-item v-if="artifact" :label="$t('Store.ModifyTheTime')">
            {{ currentArtifact.lastModified }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentArtifact && currentArtifact.manifest.layers" :label="$t('Store.NumberOfFloors')">
            {{ currentArtifact.manifest.layers.length }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentArtifact && currentArtifact.manifestConfig" :label="$t('Store.MakeADockerVersion')">
            {{ currentArtifact.manifestConfig.docker_version }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentArtifact && currentArtifact.manifestConfig" :label="$t('Store.MirrorOS')">
            <a-tag> {{ currentArtifact.manifestConfig.os }}</a-tag>
          </a-descriptions-item>
          <a-descriptions-item v-if="currentArtifact && currentArtifact.manifestConfig" :label="$t('Store.TheInfrastructure')">
            {{ currentArtifact.manifestConfig.architecture }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentArtifact && currentArtifact.manifestConfig && currentArtifact.manifestConfig.variant" :label="$t('Store.Version')">
            {{ currentArtifact.manifestConfig.variant || ''}}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentArtifact && !currentArtifact.manifestConfig" :label="$t('Store.CacheStatus')">
            {{ $t('Store.Uncached') }}
          </a-descriptions-item>
        </a-descriptions>
      </a-tab-pane>
      <a-tab-pane key="2" :tab="$t('Store.Metadata')">
        <a-table :columns="i18nMetadataColumns" :data-source="metadataList" rowKey="key" :scroll="{ x: true }">
          <div slot="type" slot-scope="type">
            <span v-for="(item, index) in metadataTypes" :key="index">
              <span v-if="type === item.value">
                <a-tag color="#87d068"> {{ item.label }} </a-tag>
              </span>
            </span>
          </div>
          <div slot="value" slot-scope="text, record">
            <span v-if="record.type === 'NUMERICAL'">
              {{ fixedNumber(record.value) }}
            </span>
            <span
              v-if="
                record.type !== 'TEXT' &&
                record.type !== 'MD' &&
                record.type !== 'JSON' &&
                record.type !== 'NUMERICAL'
              "
            >
              {{ record.value }}
            </span>
            <a-button
              type="link"
              size="small"
              v-if="record.type === 'TEXT' || record.type === 'MD'"
              @click="metadataEditorDrawerShow(record)"
            >
            {{ $t('Store.View') }}
            </a-button>
            <a-button
              type="link"
              size="small"
              v-if="record.type === 'JSON'"
              @click="metadataPrismEditorDrawerShow(record)"
            >
            {{ $t('Store.View') }}
            </a-button>
          </div>
        </a-table>
      </a-tab-pane>
      <a-tab-pane key="3" :tab="$t('Store.ConanInformation')" v-if="conanInfoVisible">
        <a-descriptions
          :title="$t('Store.Configure')"
          :column="1"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item :label="$t('Store.PackageName')">
            {{ conanInfo.recipeInfo.name }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.Version')">
            {{ conanInfo.recipeInfo.version }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.User')">
            {{ conanInfo.recipeInfo.user }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.Channel')">
            {{ conanInfo.recipeInfo.channel }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.Quote')">
            {{ conanInfo.recipeInfo.reference }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.TheAuthor')">
            {{ conanInfo.recipeInfo.author }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.Permission')">
            {{ conanInfo.recipeInfo.license }}
          </a-descriptions-item>
          <a-descriptions-item :label="URL">
            {{ conanInfo.recipeInfo.url }}
          </a-descriptions-item>
        </a-descriptions>
        <a-descriptions
          :title="$t('Store.PackageInformation')"
          :column="1"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item :label="$t('Store.NumberOfPackets')">
            {{ conanInfo.packageCount }}
          </a-descriptions-item>
        </a-descriptions>
      </a-tab-pane>
      <a-tab-pane key="4" :tab="$t('Store.ConanBagInfo')" v-if="conanPackageInfoVisible">
        <a-descriptions
          title="Settings"
          :column="1"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item :label="k"  v-for="(v, k, index) in conanPackageInfo.settings" :key="index">
            {{ v }}
          </a-descriptions-item>
        </a-descriptions>
        <a-descriptions
          title="Options"
          :column="1"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item :label="k"  v-for="(v, k, index) in conanPackageInfo.options" :key="index">
            {{ v }}
          </a-descriptions-item>
        </a-descriptions>
        <a-descriptions
          title="Requires"
          :column="1"
          :colon="false"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item :label="k"  v-for="(v, k, index) in conanPackageInfo.requires" :key="index">
            {{ v }}
          </a-descriptions-item>
        </a-descriptions>
      </a-tab-pane>
    </a-tabs>

    <hr class="my-25" />
    <a-col
      :span="24"
      v-if="
        currentArtifact && currentArtifact.snippets &&
        currentArtifact.snippets.length > 0 && 
        currentArtifact.snippets.map(e => e.code).filter(e => e).length > 0
      "
    >
      <a-card :bordered="false" class="card-billing-info">
        <div class="col-info">
          <a-descriptions
            :title="$t('Store.UseExamples') + '(' + codeParam.type + ')'"
            :column="1"
          >
            <a-descriptions-item v-if="currentArtifact">
              <prism-editor
                class="my-editor height-300"
                v-if="currentArtifact"
                v-model="codeParam.code"
                :highlight="highlighterHandle"
                :line-numbers="false"
                :readonly="true"
              ></prism-editor>
            </a-descriptions-item>
          </a-descriptions>
        </div>
        <div class="col-action">
          <a-button
            v-for="(item, index) in currentArtifact.snippets"
            :key="index"
            type="link"
            size="small"
            @click="changeCodeType(item)"
          >
            <a-avatar
              :size="20"
              shape="square"
              :src="'images/folib/' + getCodeImg(item) + '.svg'"
            />
          </a-button>
        </div>
      </a-card>
    </a-col>

    <!-- 编辑 -->
    <a-drawer
      placement="right"
      width="40%"
      :title="metadataPrismEditorDrawerTitle"
      :visible="metadataPrismEditorDrawerVisible"
      @close="metadataPrismEditorDrawerClose()"
    >
      <prism-editor
        class="metadata-prism-editor"
        style="height: 90vh"
        v-model="metadataPrismEditorDrawerValue"
        :highlight="highlighterHandle"
        :line-numbers="true"
        :readonly="true"
      >
      </prism-editor>
    </a-drawer>

    <!-- 查看 -->
    <a-drawer
      placement="right"
      width="40%"
      :title="metadataEditorDrawerTitle"
      :visible="metadataEditorDrawerVisible"
      @close="metadataEditorDrawerClose()"
    >
      <quill-editor
        class=""
        :disabled="true"
        v-model="metadataEditorDrawerValue"
        :options="quillOptions"
        style="height: 85vh"
      />
    </a-drawer>
  </div>
</template>
<script>
import store from "store";
import { fileSizeConver, formateDate } from "@/utils/layoutUtil";
import { getArtifact } from "@/api/folib";
import {  deleteArtifactMetadata, conanInfo, conanPackageInfo } from "@/api/artifact";
import { getMetadataConfiguration } from '@/api/settings'
import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css"; // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/themes/prism-tomorrow.css";
import "quill/dist/quill.core.css";
import "quill/dist/quill.snow.css";
import { quillEditor } from "vue-quill-editor";
import { hasRole, isAdmin, isAnonymous, isLogin } from "@/utils/permission";

export default {
  name: "ArtifactData",
  props: [
    "artifactName",
    "artifactPath",
    "currentArtifact",
    "repositoryType",
    "artifact",
    "folibRepository",
    "message",
  ],
  components: {
    PrismEditor,
    quillEditor,
  },
  data() {
    return {
      metadataList: [],
      metadataConfigList: [],
      metadataTypes: [
        {
          label: this.$t('Store.Number'),
          value: "NUMERICAL",
        },
        {
          label: this.$t('Store.String'),
          value: "STRING",
        },
        {
          label: this.$t('Store.Text'),
          value: "TEXT",
        },
        {
          label: "Markdown",
          value: "MD",
        },
        {
          label: "JSON",
          value: "JSON",
        },
      ],
      metadataColumns: [
        {
          i18nKey: 'Store.MetadataKey',
          title: "元数据KEY",
          dataIndex: "key",
          key: "key",
          width: 150,
        },
        {
          i18nKey: 'Store.MetadataType',
          title: "元数据类型",
          dataIndex: "type",
          key: "type",
          width: 150,
          scopedSlots: { customRender: "type" },
        },
        {
          i18nKey: 'Store.MetadataValues',
          title: "元数据值",
          dataIndex: "value",
          key: "value",
          width: 300,
          scopedSlots: { customRender: "value" },
        },
      ],
      metadataEditorDrawerTitle: undefined,
      metadataEditorDrawerVisible: false,
      metadataEditorDrawerValue: undefined,
      metadataPrismEditorDrawerTitle: undefined,
      metadataPrismEditorDrawerValue: false,
      metadataPrismEditorDrawerVisible: undefined,
      metadataInput: true,
      metadataNumber: false,
      metadataEditor: false,
      prismEditor: false,
      metadataForm: {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: undefined,
        viewShow: true,
        value: undefined,
      },
      handlerMetadataType: 1,
      codeParam: {
        type: "",
        code: null,
      },
      quillOptions: {
        modules: {
          toolbar: [
            [{ header: [1, 2, 3, false] }],
            ["bold", "italic", "underline"],
            [{ list: "ordered" }, { list: "bullet" }, "link"],
            ["clean"],
          ],
        },
      },
      metadataEnabled: false,
      conanInfo: {
          recipeInfo: {
              name: "",
              version: "",
              user: "",
              channel: "",
              reference: "",
              author: "",
              license: "",
              url: ""
          },
          packageCount: 0
      },
      conanInfoVisible: false,
      conanPackageInfo: {
        settings: {},
        options: {},
        requires: {}
      },
      conanPackageInfoVisible: false,
      selectedTag: 0,
      selectedColor: "#2db7f5"
    };
  },
  computed: {
    i18nMetadataColumns() {
      return this.metadataColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    }
  },
  created() {
    if (isLogin()){
      this.metadataShow()
    }
  },
  mounted() {
  },
  watch: {
    currentArtifact: function (val) {
      if (val && val.snippets) {
        this.changeCodeType(val.snippets[0])
      }
      this.metadataShow()
    },
    'artifactPath': function (newval, oldVal) {
      this.conanInfoReset()
      this.conanPackageInfoReset()
      this.metadataList = []
      if (this.artifact) {
        this.getMetadata()
        this.selectedTag = 0
      } else if (newval && newval.split('/').length === 5 && repositoryType === 'conan'){
        this.conanInfoVisible = true
        this.getConanInfo()
      } else if (newval && newval.split('/').length === 8 && repositoryType === 'conan'){
        this.conanPackageInfoVisible = true
        this.getConanPackageInfo()
      }
    },
  },
  methods: {
    getMetadataConfiguration () {
      getMetadataConfiguration()
        .then(res => {
          this.metadataConfigList = res
        })
        .finally(() => { })
    },
    metadataShow() {
      this.metadataEnabled = isLogin() && this.folibRepository.type !== 'group' &&
                          this.artifact &&
                          this.artifact.artifactFileExists   
      if (!(typeof(this.metadataEnabled) == 'boolean')) {
        this.metadataEnabled = false
      }
    },
    artifactTabChange(activeKey) {
      this.metadataShow()
      if (activeKey === "2") {
        this.getMetadata()
      }
    },
    getMetadata() {
      if (this.artifact) {
        this.getMetadataConfiguration()
        getArtifact(
          this.repositoryType,
          this.artifact.storageId,
          this.artifact.repositoryId,
          this.artifactPath
        ).then((res) => {
          this.handlerRespMetadata(res)
          this.$forceUpdate()
        })
      }      
    },
    handlerRespMetadata(res) {
      let metadataList = []
      if (
        res.artifact &&
        res.artifact.metadata &&
        res.artifact.metadata.length > 0
      ) {
        let metadataJson = JSON.parse(res.artifact.metadata)
        for (let key in metadataJson) {
          let flag = this.metadataConfigList.some(
            (metadataConfig) =>
              !metadataConfig.viewShow && metadataConfig.key === key
          );
          if (flag) {
            metadataJson[key].viewShow = false;
            continue
          }
          let metadata = Object.assign({}, metadataJson[key]);
          metadata.key = key;
          metadataList.push(metadata);
        }
      }
      this.metadataList = metadataList;
    },
    metadataEditorDrawerShow(metadata) {
      this.metadataEditorDrawerTitle = metadata.key;
      this.metadataEditorDrawerValue = metadata.value;
      this.metadataEditorDrawerVisible = true;
    },
    metadataEditorDrawerClose() {
      this.metadataEditorDrawerVisible = false;
    },
    metadataPrismEditorDrawerShow(metadata) {
      this.metadataPrismEditorDrawerTitle = metadata.key;
      this.metadataPrismEditorDrawerValue = metadata.value;
      this.metadataPrismEditorDrawerVisible = true;
    },
    metadataPrismEditorDrawerClose() {
      this.metadataPrismEditorDrawerVisible = false;
    },
    deleteArtifactMetadata(metadataKey) {
      let data = {
        key: metadataKey,
        storageId: this.artifact.storageId,
        repositoryId: this.artifact.repositoryId,
        artifactPath: this.artifactPath,
      };
      deleteArtifactMetadata(data)
        .then((res) => {
          this.message("删除制品元数据成功");
          this.getMetadata();
        })
        .finally(() => {});
    },
    metadataEditHandler(metadata) {
      this.$emit("metadataEditHandler", metadata);
    },
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
    changeCodeType(item) {
      if (item) {
        this.codeParam = {
          type: item.name === "Maven 2" ? "maven" : item.name.toLowerCase(),
          code: item.code,
        };
      }
    },
    getCodeImg(item) {
      return item.name === "Maven 2" ? "maven_black" : item.name.toLowerCase();
    },
    fileSizeConver(size) {
      if (size) {
        return fileSizeConver(size);
      }
    },
    formateDate(time) {
      if (time) {
        var date = new Date(time);
        var Y = date.getFullYear() + "-";
        var M =
          (date.getMonth() + 1 < 10
            ? "0" + (date.getMonth() + 1)
            : date.getMonth() + 1) + "-";
        var D =
          (date.getDate() < 10 ? "0" + date.getDate() : date.getDate()) + " ";
        var h =
          (date.getHours() < 10 ? "0" + date.getHours() : date.getHours()) +
          ":";
        var m =
          (date.getMinutes() < 10
            ? "0" + date.getMinutes()
            : date.getMinutes()) + ":";
        var s =
          (date.getSeconds() < 10
            ? "0" + date.getSeconds()
            : date.getSeconds()) + "";
        return Y + M + D + h + m + s;
      }
    },
    fixedNumber(val) {
      if (val) {
        let newVal = new Number(val);
        return newVal;
      }
      return 0;
    },
    getConanInfo() {
      let data = {
        storageId: this.artifact.storageId, 
        repositoryId: this.artifact.repositoryId, 
        artifactPath: this.artifactPath
      }
      conanInfo(data).then((res) => {
          if (res) {
            this.conanInfo = res
          }
        }).finally(() => {});
    },
    conanInfoReset() {
      this.conanInfoVisible = false
      this.conanInfo =  {
          recipeInfo: {
              name: "",
              version: "",
              user: "",
              channel: "",
              reference: "",
              author: "",
              license: "",
              url: ""
          },
          packageCount: 0
      }      
    },
    getConanPackageInfo() {
      let data = {
        storageId: this.artifact.storageId, 
        repositoryId: this.artifact.repositoryId, 
        artifactPath: this.artifactPath
      }
      conanPackageInfo(data).then((res) => {
          if (res) {
            this.conanPackageInfo = res
          }
        }).finally(() => {});
    },
    conanPackageInfoReset() {
      this.conanPackageInfoVisible = false
      this.conanPackageInfo = {
        settings: {},
        options: {},
        requires: {}
      }
    },
    clickTag(item, index) {
      this.selectedTag = 0
      getArtifact(
        this.repositoryType,
        this.artifact.storageId,
        this.artifact.repositoryId,
        this.artifactPath,
        item.digest
      ).then((res) => {
        this.$emit("setCurrentArtifact", res)
        this.selectedTag = index
        this.$forceUpdate()
      })
    },
  },
};
</script>
