<template>
  <div class="lib-view">
    <!-- Header Background Image -->
    <div class="profile-nav-bg">
        <div
          :class="[mouseEnter ? 'mouse-enter nested' : 'nested']"
          style="
            background: url(images/banner.jpg) center/cover;
            transition: all 0.3s;
          "
        ></div>
      <a-row type="flex" :md="8" :xs="4">
        <search-box @mouse="searchBoxMouseStatus" @search="search"/>
      </a-row>
    </div>
    <a-tabs
      class="tabs-sliding"
      :default-active-key="1"
      :activeKey="tabActiveKey"
      @change="tabChange($event)"
    >
      <a-tab-pane :key="1" tab="详情">
        <store
          ref="store"
          :metadataTypes="metadataTypes"
          :quillOptions="quillOptions"
          :propScanReport="scanReport"
          :successMsg="successMsg"
          :formateDate="formateDate"
          @openDetial="openDetial"
        />
      </a-tab-pane>
      <a-tab-pane :key="2" tab="统计" v-if="$store.state.user.token">
        <safe
          v-if="tabActiveKey == 2"
          :folibRepository="folibRepository"
          :vulnerabilityColumns="vulnerabilityColumns"
        />
      </a-tab-pane>
      <a-button v-if="settingsEnabled" slot="tabBarExtraContent" icon="setting" class="repository-setting" size="small" @click="settingDrawerShow()" />
    </a-tabs>
    <!-- / Header Background Image -->

    <SettingsDrawer :folibRepository="this.folibRepository" :settingVisible="settingVisible" @settingDrawerClose="settingDrawerClose"></SettingsDrawer>

    <!-- docker -->
    <a-drawer
      placement="right"
      width="45%"
      :title="currentTreeNode.name"
      :visible="viewCodeVisible"
      @close="closeViewCodeDialog"
    >
      <div class="mx-auto m-50">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree
              v-if="currentFileDetial && currentFileDetial.listTree"
              :replaceFields="{ title: 'name', children: 'children' }"
              :tree-data="currentFileDetial.listTree"
            />
          </a-card>
          <prism-editor
            class="my-editor height-300"
            v-if="
              currentFileDetial &&
              viewCodes &&
              folibRepository.layout !== 'Docker'
            "
            v-model="viewCodes"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          ></prism-editor>

          <a-tabs
            v-if="
              currentFileDetial &&
              currentManifest &&
              folibRepository.layout === 'Docker'
            "
            class="tabs-sliding"
            default-active-key="1"
          >
            <a-tab-pane key="1" tab="Layers">
              <a-timeline>
                <template v-if="currentManifest.config[index]">
                  <a-timeline-item
                    color="primary"
                    v-for="(key, index) in currentManifest.config"
                    :key="index"
                  >
                    {{ index }}
                    <p>
                      {{ currentManifest.config[index] }}
                    </p>
                  </a-timeline-item>
                </template>
              </a-timeline>
            </a-tab-pane>
            <a-tab-pane key="2" tab="制作历史">
              <a-timeline>
                <template v-if="currentManifest.history[index]">
                  <a-timeline-item
                    color="primary"
                    v-for="(key, index) in currentManifest.history"
                    :key="index"
                  >
                    {{ formateDate(key.created) }}
                    <p>
                      {{ key.created_by }}
                    </p>
                  </a-timeline-item>
                </template>
              </a-timeline>
            </a-tab-pane>
          </a-tabs>
        </div>
      </div>
    </a-drawer>

    <VunlerabilityReport :report="scanReport.report" :reportVisible="detialVisible" @closeReport="closeReport"/>

  </div>
</template>

<script>
import storage from "store";
import CardPackageTree from "@/components/Cards/CardPackageTree";
import CardProfileInformation from "../../components/Cards/CardProfileInformation";
import Vulnerability from "@/components/Vulnerabilities/Vulnerability";
import {
  getLayoutType,
  getFileType,
  fileSizeConver,
  formateDate,
} from "@/utils/layoutUtil";
import {
  viewArtifactFile,
  getLibraryFilter
} from "@/api/folib";
import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css"; // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/themes/prism-tomorrow.css";
import SearchBox from "@/components/Tools/SearchBox";
import zhCN from "ant-design-vue/es/locale/zh_CN";
import "quill/dist/quill.core.css";
import "quill/dist/quill.snow.css";
import { quillEditor } from "vue-quill-editor";
import Store from "./components/Store/index.vue";
import Safe from "./components/Safe/index.vue";
import SettingsDrawer from "./components/Repository/SettingsDrawer.vue";
import { hasRole, isAdmin, hasPermission, isLogin } from "@/utils/permission";
import VunlerabilityReport from '@/components/Vulnerabilities/VunlerabilityReport'

export default {
  inject: ["reload"],
  components: {
    CardPackageTree,
    CardProfileInformation,
    PrismEditor,
    SearchBox,
    Vulnerability,
    quillEditor,
    Store,
    Safe,
    SettingsDrawer,
    VunlerabilityReport,
  },
  data() {
    return {
      scan: {
        id: "",
        repository: "",
        storage: "",
        onScan: false,
        scanRule: null,
        layout: null,
      },
      viewCodeVisible: false,
      repositoryType: null,
      folibRepository: {},
      baseUrl: "",
      currentTreeNode: {},
      currentFileDetial: null,
      currentManifest: {},
      codeParam: {
        type: "",
        code: null,
      },
      viewCodes: null,
      mouseEnter: false,
      snippets: [],
      // Table columns
      columns: [
        {
          title: "制品路径",
          dataIndex: "path",
          scopedSlots: { customRender: "path" },
          width: 550,
        },
        {
          title: "创建时间",
          dataIndex: "created",
          sorter: true,
          sortDirections: ["descend", "ascend"],
          scopedSlots: { customRender: "created" },
          width: 200,
        },
        {
          title: "最近使用时间",
          dataIndex: "lastUsed",
          sorter: true,
          scopedSlots: { customRender: "lastUsed" },
          width: 200,
        },
        {
          title: "下载次数",
          dataIndex: "downloadCount",
          sorter: true,
          scopedSlots: { customRender: "created" },
          width: 200,
        },
        {
          title: "制品大小",
          dataIndex: "sizeInBytes",
          sorter: true,
          scopedSlots: { customRender: "sizeInBytes" },
          width: 200,
        },
      ],
      scanReport: {
        show: false,
        report: [],
        vulnerabilitesCount: 0,
        critical: 0,
        high: 0,
        medium: 0,
        low: 0,
      },
      detialVisible: false,
      vulnerColumns: [
        {
          title: "CVE编号",
          dataIndex: "name",
          scopedSlots: { customRender: "name" },
        },
        {
          title: "漏洞等级",
          dataIndex: "highestSeverityText",
          scopedSlots: { customRender: "highestSeverityText" },
        },
        {
          title: "CvssV2评分",
          dataIndex: "cvssV2",
          scopedSlots: { customRender: "v2_exploitabilityScore" },
        },
        {
          title: "CvssV3评分",
          dataIndex: "cvssV3",
          scopedSlots: { customRender: "v3_exploitabilityScore" },
        },
        {
          title: "引入版本",
          scopedSlots: { customRender: "versionStartIncluding" },
        },
        {
          title: "建议修复版本",
          scopedSlots: { customRender: "versionEndExcluding" },
        },
      ],
      vulnerabilityColumns: [
        {
          title: "漏洞编号",
          dataIndex: "uuid",
          scopedSlots: { customRender: "uuid" },
        },
        {
          title: "引入时间",
          dataIndex: "created",
          scopedSlots: { customRender: "created" },
          align: "center",
        },
        {
          title: "CvssV2评分",
          dataIndex: "cvssV2Score",
          scopedSlots: { customRender: "cvssV2Score" },
          align: "center",
        },
        {
          title: "CvssV2漏洞等级",
          dataIndex: "cvssV2Severity",
          scopedSlots: { customRender: "cvssV2Severity" },
          align: "center",
        },
        {
          title: "CvssV3评分",
          dataIndex: "cvssV3Score",
          scopedSlots: { customRender: "cvssV3Score" },
          align: "center",
        },
        {
          title: "CvssV3漏洞等级",
          dataIndex: "cvssV3Severity",
          scopedSlots: { customRender: "cvssV3Severity" },
          align: "center",
        },
        {
          title: "最高漏洞等级",
          dataIndex: "highestSeverityText",
          scopedSlots: { customRender: "highestSeverityText" },
          align: "center",
        },
        {
          title: "建议修复版本",
          dataIndex: "versionEndExcluding",
          scopedSlots: { customRender: "versionEndExcluding" },
        },
        {
          title: "操作",
          dataIndex: "operation",
          scopedSlots: { customRender: "operation" },
        },
      ],
      tabActiveKey: 1,
      locale: zhCN,
      operationForm: this.$form.createForm(this, { name: "operation_form" }),
      repositories: [],
      storages: [],
      custom: false,
      enabled: true,
      prismEditor: false,
      metadataTypes: [
        {
          label: "数字",
          value: "NUMERICAL",
        },
        {
          label: "字符串",
          value: "STRING",
        },
        {
          label: "文本",
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
      settingsEnabled: false,
      settingVisible: false,
    }
  },
  created() {
    this.createData()
    this.getStorage(this.folibRepository.storageId)
  },
  methods: {
    searchBoxMouseStatus(bool) {
      this.mouseEnter = bool;
    },
    search(value, searchType, type) {
      this.tabActiveKey = 1
      this.$refs.store.search(value, searchType, type)
    },
    createData() {
      //上个页面通过缓存传参，目的防止页面刷新，路由数据消失
      const params = storage.get("libView_repository");
      this.folibRepository = params.item;
      if (!this.folibRepository || this.folibRepository.type !== "hosted") {
        this.enabled = false;
      }
      this.baseUrl = params.baseUrl;
      this.repositoryType = this.getLayoutTypeHandle();
    },
    getLayoutTypeHandle() {
      return getLayoutType(this.folibRepository);
    },
    getFileType(name) {
      if (name) {
        return getFileType(name);
      }
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
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
    closeViewCodeDialog() {
      this.viewCodeVisible = false;
      this.viewCodes = null;
    },
    viewCodeHandle() {
      if (this.folibRepository.layout !== "Docker") {
        if (this.currentFileDetial && !this.currentFileDetial.listTree) {
          viewArtifactFile(this.currentTreeNode.url).then((res) => {
            if ("string" === typeof res && res.startsWith("PK")) {
              this.viewCodes = undefined;
            } else if ("object" === typeof res) {
              this.viewCodes = JSON.stringify(res);
            } else {
              this.viewCodes = res;
            }
          });
        }
      } else {
        // this.viewCodes=this.currentManifest.config
      }

      this.viewCodeVisible = true;
    },
    closeReport() {
      this.detialVisible = false;
    },
    getImage(ecosystem) {
      return ecosystem ? ecosystem : this.getLayoutTypeHandle();
    },
    tabChange(activeKey) {
      this.tabActiveKey = activeKey;
      if (activeKey == 2) {
        if (this.$refs.vulnerability) {
          this.$refs.vulnerability.getVulnerabilityPage();
        }
      }
    },
    successMsg(message) {
      if (!message) {
        message = "操作成功";
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    customChange(value) {
      this.custom = value;
      if (!value) {
        this.$nextTick(() => {
          if (this.$refs.operationForm) {
            this.operationForm.setFieldsValue({
              path: this.currentTreeNode.artifactPath,
            });
          }
        });
      }
    },
    openDetial(data){
      if(JSON.stringify(data)!==JSON.stringify(this.scanReport)){
        Object.assign(this.scanReport,data)
      }
      this.detialVisible = true
    },
    settingDrawerShow() {
      this.settingVisible = true
    },
    settingDrawerClose() {
      this.settingVisible = false
    },
    getStorage(id) {
      getLibraryFilter(id).then(response => {
        this.settingsEnabled = isAdmin() || response.admin === this.$store.state.user.name
      })
    },
  },
};
</script>

<style lang="scss" scoped>
$md: 768px;

.lib-view::v-deep {
  .profile-nav-bg {
    display: flex;
    justify-content: center;
    align-items: center;
    color: #fafafa;
    position: relative;
    overflow: hidden;
    width: 100%;
  }

  .statistics-bg {
    height: 75px !important;
  }

  .my-editor {
    background: #fafafa;
    color: #595959;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  // optional
  .prism-editor__textarea:focus {
    outline: none;
  }

  // not required:
  .height-80 {
    height: 80px;
  }

  .mouse-enter {
    transform: scale(1.3);
    transition: all 0.3s;
  }

  .nested {
    position: absolute;
    left: 0;
    right: 0;
    top: 0;
    bottom: 0;
  }

  //search列表
  .table-avatar-info {
    display: flex;
    align-items: center;
  }

  .table-avatar-info .ant-avatar {
    margin-right: 8px;
  }

  // Using vuejs "Deep Selectors"
  .table-avatar-info::v-deep .ant-avatar-string {
    font-size: 12px;
  }

  .btn-status::v-deep .anticon {
    line-height: 0;
  }

  .collapse-panel-header-info {
    display: inline-block;
  }

  .collapse-panel-header-info .file-name,
  .bug-count {
    margin-right: 10px;
  }

  .collapse-panel-header-info .bug-count {
    vertical-align: middle;
    margin-left: 2.5px;
  }

  .repository-affix {
    margin-top: 50px;
  }

  .card-profile-head {
    margin: -53px 0px 24px;
  }

  .widget-2 .icon svg path {
    fill: #ffffff;
  }

  .vulnerability-count {
    cursor: pointer;
  }

  .search-column-path {
    white-space: pre-line;
    width: calc(100% - 24px);
  }

  .metadata-descriptions .ant-descriptions-item-content {
    width: 100%;
    background-color: green;
  }
}

.d-popconfirm {
  height: 34px;
  font-size: 12px;
  font-weight: 600;
  margin-right: 20px;
}

.d-popconfirm > svg + span {
  vertical-align: middle;
  display: inline-block;
  transition: margin-left 0.3s cubic-bezier(0.645, 0.045, 0.355, 1);
  pointer-events: none;
}

.d-popconfirm svg {
  vertical-align: middle;
  margin-right: 5px;
}

.metadata-prism-editor {
  background: black;
  border-radius: 4px;
  color: #e8e8e8;
  height: 300px;
  font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
  font-size: 12px;
  line-height: 1.5;
  padding: 5px;
}
.repository-setting {
  margin: 0 5px;
  margin-top: 12px;
}
</style>
