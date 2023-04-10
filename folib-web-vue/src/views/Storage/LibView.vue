<template>
  <div class="lib-view">
    <!-- Header Background Image -->
    <div class="profile-nav-bg">
      <div
        :class="[mouseEnter ? 'mouse-enter nested' : 'nested']"
        style="
          background: url(images/bg-profile.jpg) center/cover;
          transition: all 0.3s;
        "
      />
      <a-row type="flex" :md="8" :xs="4">
        <search-box @mouse="searchBoxMouseStatus" @search="search" />
      </a-row>
    </div>
    <a-tabs
      class="tabs-sliding"
      :default-active-key="1"
      :activeKey="tabActiveKey"
      @change="tabChange($event)"
    >
      <a-tab-pane :key="1" tab="仓库">
        <store
          ref="store"
          :metadataTypes="metadataTypes"
          :quillOptions="quillOptions"
          :searchType="searchType"
          :propScanReport="scanReport"
          :successMsg="successMsg"
          :formateDate="formateDate"
          @searchDataHandle="searchDataHandle"
          @openDetial="openDetial"
        />
      </a-tab-pane>
      <a-tab-pane :key="2" tab="安全">
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

    <!-- User Profile Card -->

    <a-drawer
      placement="right"
      width="65%"
      title="制品详情"
      :visible="artifactVisible"
      @close="artifactVisible = false"
      :zIndex="100"
    >
      <a-card
        :bordered="false"
        class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
        :headStyle="{ paddingRight: 0 }"
      >
        <template #title>
          <h6 class="font-semibold m-0">
            <a-avatar
              :size="24"
              shape="square"
              :src="
                folibRepository.layout === 'Docker'
                  ? 'images/folib/docker-s.svg'
                  : 'images/folib/' +
                    getFileType(
                      searchDataCurrentSelect
                        ? searchDataCurrentSelect.path
                        : ''
                    ) +
                    '.svg'
              "
            />
            {{ searchDataCurrentSelect ? searchDataCurrentSelect.path : "" }}
            <span
              class="ml-auto"
              v-if="scanReport.show"
              @click="detialVisible = true"
            >
              <a-space :size="1" class="avatar-chips">
                <template v-if="scanReport.vulnerabilitesCount > 0">
                  <a-tooltip>
                    <template slot="title">严重</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/critical.svg'" />
                      <span class="mb-0 text-dark">{{
                        scanReport.critical
                      }}</span>
                    </div>
                  </a-tooltip>

                  <a-tooltip>
                    <template slot="title">高危</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/high.svg'" />
                      <span class="mb-0 text-dark">{{ scanReport.high }}</span>
                    </div>
                  </a-tooltip>

                  <a-tooltip>
                    <template slot="title">中危</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/medium.svg'" />
                      <span class="mb-0 text-dark">{{
                        scanReport.medium
                      }}</span>
                    </div>
                  </a-tooltip>

                  <a-tooltip>
                    <template slot="title">低危</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/low.svg'" />
                      <span class="mb-0 text-dark">{{ scanReport.low }}</span>
                    </div>
                  </a-tooltip>
                </template>
                <template v-else>
                  <a-tooltip>
                    <template slot="title">健康</template>
                    <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
                  </a-tooltip>
                </template>
              </a-space>
            </span>
          </h6>
        </template>
        <a-button type="link" slot="extra" @click="searchViewCodeHandle()">
          预览
          <a-icon :size="24" shape="square" type="eye"></a-icon>
        </a-button>
        <a
          class="text-dark"
          :href="searchDataCurrentSelect ? searchDataCurrentSelect.url : ''"
          target="_blank"
          >{{ searchDataCurrentSelect ? searchDataCurrentSelect.url : "" }}</a
        >
        <hr class="my-25" />
        <a-descriptions
          title="基本信息"
          :column="1"
          v-if="searchDataCurrentSelect"
        >
          <a-descriptions-item label="所属空间">
            {{ searchDataCurrentSelect.storageId }}
          </a-descriptions-item>
          <a-descriptions-item label="所属仓库">
            {{ searchDataCurrentSelect.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item label="名称">
            {{ searchDataCurrentSelect.path }}
          </a-descriptions-item>
          <a-descriptions-item label="文件大小">
            {{ fileSizeConver(searchDataCurrentSelect.sizeInBytes) }}
          </a-descriptions-item>
          <a-descriptions-item label="修改时间">
            {{ searchDataCurrentSelect.lastUpdated }}
          </a-descriptions-item>
          <a-descriptions-item label="最近使用时间">
            {{ searchDataCurrentSelect.lastUsed }}
          </a-descriptions-item>
          <a-descriptions-item v-if="searchDataCurrentSelect" label="下载次数">
            {{ searchDataCurrentSelect.downloadCount }}
          </a-descriptions-item>
          <template v-if="searchDataCurrentSelect && searchDataCurrentSelect.checksums" >
            <a-descriptions-item :label="key" v-for="(value, key, index) in searchDataCurrentSelect.checksums" :key="index">
              {{ value}}
            </a-descriptions-item>
          </template>
        </a-descriptions>
        <hr class="my-25" />

        <a-col
          :span="24"
          v-if="
            searchDataCurrentSelect &&
            searchDataCurrentSelect.snippets &&
            searchDataCurrentSelect.snippets.length > 0
          "
        >
          <a-card :bordered="false" class="card-billing-info">
            <div class="col-info">
              <a-descriptions
                :title="'使用示例(' + codeParam.type + ')'"
                :column="1"
              >
                <a-descriptions-item v-if="searchDataCurrentSelect">
                  <prism-editor
                    class="my-editor height-300"
                    v-if="searchDataCurrentSelect"
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
                v-for="(item, index) in this.searchDataCurrentSelect.snippets"
                :key="index"
                type="link"
                size="small"
                @click="changeCodeTye(item)"
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
      </a-card>
    </a-drawer>

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

    <!-- 搜索预览 -->
    <a-drawer
      placement="right"
      width="45%"
      v-if="searchDataCurrentSelect"
      :title="searchDataCurrentSelect.path"
      :visible="searchViewCodeVisible"
      @close="closeSearchviewCodeDialog"
    >
      <div class="mx-auto m-50" style="max-width: 1000px">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree
              v-if="searchDataCurrentSelect && searchDataCurrentSelect.treeNode"
              :replaceFields="{ title: 'name', children: 'children' }"
              :tree-data="searchDataCurrentSelect.treeNode"
            />
          </a-card>
          <prism-editor
            class="my-editor height-300"
            v-if="searchDataCurrentSelect && searchViewCodes"
            v-model="searchViewCodes"
            :highlight="highlighterHandle"
            :line-numbers="false"
            :readonly="true"
          >
          </prism-editor>
        </div>
      </div>
    </a-drawer>

    <a-drawer
      placement="right"
      width="65%"
      title="报告详情"
      :visible="detialVisible"
      @close="closeDialog"
    >
      <a-collapse default-active-key="1" :bordered="false" accordion>
        <template #expandIcon="props">
          <a-icon type="caret-right" :rotate="props.isActive ? 90 : 0" />
        </template>
        <a-collapse-panel
          v-for="(item, index) in scanReport.report"
          :key="index"
          style="
            background: #f7f7f7;
            border-radius: 4px;
            margin-bottom: 24px;
            border: 0;
            overflow: hidden;
          "
        >
          <template slot="header">
            <div class="collapse-panel-header-info">
              <span class="file-name">{{ item.fileName }}</span>
              <a-tooltip v-if="item.vulnerabilitiesCount > 0">
                <template slot="title">漏洞数量</template>
                <a-avatar :size="24" :src="'images/folib/bug.svg'" />
                <span class="mb-0 text-dark bug-count">{{
                  item.vulnerabilitiesCount
                }}</span>
              </a-tooltip>
              <a-tooltip v-else>
                <template slot="title">健康</template>
                <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
              </a-tooltip>
            </div>
          </template>
          <a-card
            :bordered="false"
            class="card-order header-solid mb-24 mx-auto mt-20 mb-50"
            :bodyStyle="{ paddingTop: 0 }"
          >
            <template #title>
              <h6 class="mb-0">{{ item.fileName }}</h6>
            </template>
            <a-row :gutter="[24]" type="flex">
              <a-col :span="24" :md="16">
                <p class="mb-0">
                  该依赖含有
                  <strong>{{ item.evidence.length }}</strong>
                  个风险凭证，并在扫描检测中发现
                  <strong>{{ item.vulnerabilitiesCount }}</strong
                  >个漏洞
                </p>
                <p class="mb-0">
                  MD5: <strong>{{ item.md5sum }}</strong>
                </p>
                <p class="mb-0">
                  SHA256: <strong>{{ item.sha256sum }}</strong>
                </p>
              </a-col>
              <a-col :span="24" :md="8" class="ml-auto text-right">
                <p class="mb-0">
                  版本号: <strong>{{ item.version }}</strong>
                </p>
              </a-col>
            </a-row>
            <hr class="gradient-line" />

            <a-row
              :gutter="[24]"
              type="flex"
              class="order-products"
              align="middle"
            >
              <a-col :span="24" :md="12">
                <div class="d-flex">
                  <a-avatar
                    class="mr-15"
                    :src="'images/folib/' + getImage(item.ecosystem) + '.svg'"
                    shape="square"
                    :size="80"
                  />
                  <div>
                    <h6 class="mb-0 mt-10 font-semibold">{{ item.name }}</h6>
                    <p class="mb-15">
                      License: <strong>{{ item.license }}</strong>
                    </p>
                    <a-tag class="ant-tag-success font-semibold">{{
                      item.ecosystem
                    }}</a-tag>
                  </div>
                </div>
              </a-col>
              <a-col :span="24" :md="12" class="ml-auto text-right">
                <p>{{ item.description }}</p>
              </a-col>
            </a-row>

            <hr class="gradient-line" />

            <a-row :gutter="[24]" type="flex">
              <a-col :span="24" :md="24" :lg="24">
                <a-table
                  :columns="vulnerColumns"
                  :data-source="item.vulnerabilities"
                  :pagination="false"
                  :row-key="(r, i) => i.toString()">
                  <a-row
                    slot="expandedRowRender"
                    :gutter="[24, 24]"
                    slot-scope="record"
                  >
                    <a-col :span="24">
                      <a-card :bordered="false" class="card-billing-info">
                        <div class="col-info">
                          <a-descriptions
                            :title="record.references.length + '个参考信息'"
                            :column="1"
                          >
                            <a-descriptions-item label="说明">
                              以下信息均来自于开源社区
                            </a-descriptions-item>
                            <a-descriptions-item label="相关信息链接">
                              <p
                                v-for="(ritem, index1) in record.references"
                                :key="index1"
                              >
                                {{ ritem.url }}
                              </p>
                            </a-descriptions-item>
                          </a-descriptions>
                        </div>
                      </a-card>
                    </a-col>
                  </a-row>
                  <template slot="name" slot-scope="text, record">
                    <div>
                      <a>
                        <h6 class="m-0">
                          {{ record.name }}
                        </h6>
                      </a>
                    </div>
                  </template>
                  <template
                    slot="highestSeverityText"
                    slot-scope="highestSeverityText"
                  >
                    <div class="table-avatar-info">
                      <a-avatar
                        v-if="
                          ['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(
                            highestSeverityText
                          ) != -1
                        "
                        :size="24"
                        :src="
                          'images/folib/' +
                          highestSeverityText.toLowerCase() +
                          '.svg'
                        "
                      />
                      <a-avatar v-else shape="circle" :size="24">{{
                        highestSeverityText.slice(0, 1)
                      }}</a-avatar>
                      <div class="avatar-info">
                        <p class="mb-0 text-dark">
                          {{
                            highestSeverityText === "CRITICAL"
                              ? "严重"
                              : highestSeverityText === "MEDIUM"
                              ? "中危"
                              : highestSeverityText === "HIGH"
                              ? "高危"
                              : highestSeverityText === "LOW"
                              ? "低危"
                              : highestSeverityText
                          }}
                        </p>
                      </div>
                    </div>
                  </template>
                  <template
                    slot="v2_exploitabilityScore"
                    slot-scope="text, record"
                    >{{ record.cvssV2.score }}</template
                  >
                  <template
                    slot="v3_exploitabilityScore"
                    slot-scope="text, record"
                    >{{ record.cvssV3.baseScore }}</template
                  >
                  <template
                    slot="versionStartIncluding"
                    slot-scope="text, record"
                    >{{
                      record.matchedVulnerableSoftware.versionStartIncluding
                    }}</template
                  >
                  <template
                    slot="versionEndExcluding"
                    slot-scope="text, record"
                    >{{
                      record.matchedVulnerableSoftware.versionEndExcluding
                    }}</template
                  >
                </a-table>
              </a-col>
            </a-row>
          </a-card>
        </a-collapse-panel>
      </a-collapse>
    </a-drawer>
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
import { getMetadataConfiguration } from "@/api/settings";
import {
  getArtifact,
  viewArtifactFile,
  repositoryVulnerabilityStatistics,
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
import { hasRole, isAdmin, hasPermission } from "@/utils/permission";

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
      isNotSearch: false,
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
      searchData: [],
      searchDataCurrentSelect: {},
      searchViewCodeVisible: false,
      searchViewCodes: null,
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
      vulnerabilityStatistics: {
        artifactCount: 0,
        downloadCount: 0,
        dependencyCount: 0,
        vulnerabilityCount: 0,
        whiteCount: 0,
        blackCount: 0,
      },
      tabActiveKey: 1,
      artifactVisible: false,
      locale: zhCN,
      operationForm: this.$form.createForm(this, { name: "operation_form" }),
      repositories: [],
      storages: [],
      custom: false,
      enabled: true,
      searchType: 1,
      prismEditor: false,
      metadataList: [],
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
      metadataConfigList: [],
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
    this.repositoryVulnerabilityStatistics()
    this.getMetadataConfiguration()
    this.getStorage(this.folibRepository.storageId)
  },
  methods: {
    searchBoxMouseStatus(bool) {
      this.mouseEnter = bool;
    },
    handlerSearchType(searchType) {
      this.searchType = searchType;
      this.$forceUpdate();
    },
    search(value, page) {
      this.tabActiveKey = 1;
      this.$refs.store.search(value, page);
    },
    searchDataHandle(item) {
      this.searchDataCurrentSelect = item;
      if (
        this.searchDataCurrentSelect &&
        this.searchDataCurrentSelect.snippets
      ) {
        this.changeCodeTye(this.searchDataCurrentSelect.snippets[0]);
      }
      this.scanReport = {
        show: false,
        report: [],
        vulnerabilitesCount: 0,
        critical: 0,
        high: 0,
        medium: 0,
        low: 0,
      };
      getArtifact(
        this.repositoryType,
        item.storageId,
        item.repositoryId,
        item.artifactPath
      ).then((res) => {
        let artifact = res.artifact;
        if (artifact && artifact.safeLevel === "scanComplete") {
          this.scanReport.show = true;
          this.scanReport.vulnerabilitesCount = artifact.vulnerabilitiesCount;
          this.scanReport.critical = artifact.criticalVulnerabilitiesCount;
          this.scanReport.high = artifact.highVulnerabilitiesCount;
          this.scanReport.medium = artifact.mediumVulnerabilitiesCount;
          this.scanReport.low = artifact.lowVulnerabilitiesCount;
          this.scanReport.report = JSON.parse(artifact.report);
        }
      });
      this.artifactVisible = true;
    },
    closeSearchviewCodeDialog() {
      this.searchViewCodeVisible = false;
      this.searchViewCodes = null;
    },
    changeCodeTye(item) {
      if (item) {
        this.codeParam = {
          type: item.name === "Maven 2" ? "maven" : item.name.toLowerCase(),
          code: item.code,
        };
      }
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
      this.isNotSearch = false;
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
    searchViewCodeHandle() {
      if (
        this.searchDataCurrentSelect &&
        !this.searchDataCurrentSelect.treeNode
      ) {
        viewArtifactFile(this.searchDataCurrentSelect.url).then((res) => {
          if ("string" === typeof res && res.startsWith("PK")) {
            this.searchViewCodes = undefined;
          } else if ("object" === typeof res) {
            this.searchViewCodes = JSON.stringify(res);
          } else {
            this.searchViewCodes = res;
          }
        });
      }
      this.searchViewCodeVisible = true;
    },
    getCodeImg(item) {
      return item.name === "Maven 2" ? "maven_black" : item.name.toLowerCase();
    },
    closeDialog() {
      this.detialVisible = false;
    },
    getImage(ecosystem) {
      return ecosystem ? ecosystem : this.getLayoutTypeHandle();
    },
    repositoryVulnerabilityStatistics() {
      repositoryVulnerabilityStatistics({
        storageId: this.folibRepository.storageId,
        repositoryId: this.folibRepository.id,
      }).then((res) => {
        this.vulnerabilityStatistics = res;
      });
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
    getMetadataConfiguration() {
      getMetadataConfiguration()
        .then((res) => {
          this.metadataConfigList = res;
        })
        .finally(() => {});
    },
    handlerRespMetadata(res) {
      let metadataList = [];
      if (
        res.artifact &&
        res.artifact.metadata &&
        res.artifact.metadata.length > 0
      ) {
        let metadataJson = JSON.parse(res.artifact.metadata);
        for (let key in metadataJson) {
          let flag = this.metadataConfigList.some(
            (metadataConfig) =>
              !metadataConfig.viewShow && metadataConfig.key === key
          );
          if (flag) {
            metadataJson[key].viewShow = false;
          }
          let metadata = Object.assign({}, metadataJson[key]);
          metadata.key = key;
          metadataList.push(metadata);
        }
      }
      this.metadataList = metadataList;
    },
    openDetial(data){
      if(JSON.stringify(data)!==JSON.stringify(this.scanReport)){
        Object.assign(this.scanReport,data)
      }
      this.detialVisible = true
    },
    getMetadata() {
      getArtifact(
        this.repositoryType,
        this.currentTreeNode.storageId,
        this.currentTreeNode.repositoryId,
        this.currentTreeNode.artifactPath
      ).then((res) => {
        this.handlerRespMetadata(res);
        this.$forceUpdate();
      });
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
