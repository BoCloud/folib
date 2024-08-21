<template>
  <div class="artifact-base-data">
    <a-tabs default-active-key="1" @change="artifactTabChange">
      <a-tab-pane key="1" :tab="$t('Store.BasicInformation')">
        <a-descriptions
          v-if="folibRepository.layout !== 'Docker'"
          title=""
          :column="1"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item :label="$t('Store.OwningSpace')">
            {{ currentTreeNode.storageId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.OwnedWarehouse')">
            {{ currentTreeNode.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.Name')">
            {{ currentTreeNode.name }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.ThePath')">
            {{ currentTreeNode.artifactPath }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.FileSize')">
            {{ fileSizeConver(currentTreeNode.size) }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.ModifyTheTime')">
            {{ formateDate(currentTreeNode.lastModified) }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" :label="$t('Store.LastUsedTime')">
            {{ currentFileDetial.lastUsedTime }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" :label="$t('Store.ScanTime')">
            {{ currentFileDetial.scanTime }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" :label="$t('Store.DownloadTimes')">
            {{ currentFileDetial.downloadCount }}
          </a-descriptions-item>
          <template v-if="currentFileDetial && currentFileDetial.artifact && currentFileDetial.artifact.checksums" >
            <a-descriptions-item :label="key" v-for="(value, key, index) in currentFileDetial.artifact.checksums" :key="index" span="2">
              {{ value }}
            </a-descriptions-item>
          </template>
        </a-descriptions>
        <div v-if="currentFileDetial && currentFileDetial.manifest && currentFileDetial.manifest.manifests">
          <a-tag class="mb-10" :color="index === selectedTag? selectedColor : ''" v-for="(item, index) in currentFileDetial.manifest.manifests" :key="index" @click="clickTag(item, index)">
            <a> {{ item.platform.os + '/' + item.platform.architecture + (item.platform.variant? '/' + item.platform.variant : '') }} </a>
          </a-tag>
        </div>
        <a-descriptions
          v-if="folibRepository.layout === 'Docker'"
          title=""
          :column="1"
        >


          <a-descriptions-item :label="$t('Store.OwningSpace')">
            {{ currentTreeNode.storageId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Store.OwningSpace')">
            {{ currentTreeNode.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" :label="$t('Store.ImageName')">
            {{ currentFileDetial.imageName }}
          </a-descriptions-item>
          <a-descriptions-item :label="currentFileDetial ? $t('Store.VersionNumber') : $t('Store.Name')">
            {{ currentTreeNode.name }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" :label="$t('Store.FileSize')">
            {{ fileSizeConver(currentFileDetial.size) }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="SHA-256">
            {{ currentFileDetial.sha256 }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" :label="$t('Store.ModifyTheTime')">
            {{ currentFileDetial.lastModified }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" :label="$t('Store.ScanTime')">
            {{ currentFileDetial.scanTime }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial && currentFileDetial.manifest.layers" :label="$t('Store.NumberOfFloors')">
            {{ currentFileDetial.manifest.layers.length }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial && currentFileDetial.manifestConfig" :label="$t('Store.MakeADockerVersion')">
            {{ currentFileDetial.manifestConfig.docker_version }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial && currentFileDetial.manifestConfig" :label="$t('Store.MirrorOS')">
            <a-tag> {{ currentFileDetial.manifestConfig.os }}</a-tag>
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial && currentFileDetial.manifestConfig" :label="$t('Store.TheInfrastructure')">
            {{ currentFileDetial.manifestConfig.architecture }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial && currentFileDetial.manifestConfig && currentFileDetial.manifestConfig.variant" :label="$t('Store.Version')">
            {{ currentFileDetial.manifestConfig.variant || ''}}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial && !currentFileDetial.manifestConfig" :label="$t('Store.CacheStatus')">
            {{ $t('Store.Uncached') }}
          </a-descriptions-item>
        </a-descriptions>
      </a-tab-pane>
      <a-tab-pane key="2" :tab="$t('Store.Metadata')">
        <a v-if="metadataEnabled" @click="metadataHandler()">
          <a-tooltip>
            <template slot="title">{{ $t('Store.Create') }}</template>
            <a-icon type="plus-circle" theme="filled" class="ml-30"
              :style="{ fontSize: '28px', color: '#1890FF' }" />
          </a-tooltip>
        </a>
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
          <div slot="operation" slot-scope="text, record">
            <div class="col-action" v-if="metadataEnabled">
              <a-popconfirm
                :title="$t('Store.SuerDelete')"
                okType="danger"
                :ok-text="$t('Store.Confirm')"
                :cancel-text="$t('Store.Cancel')"
                @confirm="deleteArtifactMetadata(record.key)"
              >
                <a-button type="link" size="small">
                  <svg
                    width="16"
                    height="16"
                    viewBox="0 0 20 20"
                    fill="none"
                    xmlns="http://www.w3.org/2000/svg"
                  >
                    <path
                      class="fill-danger"
                      fill-rule="evenodd"
                      clip-rule="evenodd"
                      d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                      fill="#111827"
                    />
                  </svg>
                  <span class="text-danger">DELETE</span>
                </a-button>
              </a-popconfirm>
              <a-button
                type="link"
                size="small"
                @click="metadataEditHandler(record)"
              >
                <svg
                  width="16"
                  height="16"
                  viewBox="0 0 20 20"
                  fill="none"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <path
                    class="fill-muted"
                    d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                    fill="#111827"
                  />
                  <path
                    class="fill-muted"
                    d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                    fill="#111827"
                  />
                </svg>
                <span class="text-dark">EDIT</span>
              </a-button>
            </div>
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
      <a-tab-pane key="5" :tab="$t('Store.BomInformation')" v-if="currentFileDetial && currentFileDetial.artifact && currentFileDetial.bom">
        <div class="ml-20 mb-10">          
          <a @click="handleGoDetail()"><a-icon type="link" /> {{ $t('Store.ViewDetail') }}</a>
        </div>
        <div class="bar" v-if="projectInfo && projectInfo.metrics">
          <div class="card-inner">
            <div class="bar-card">
              <div class="callout b-severity-critical">
                <div class="text">
                  <div class="text-muted">{{ $t("Store.Seriousness") }}</div>
                  <strong>{{ projectInfo.metrics.critical }}</strong>
                </div>
              </div>
            </div>
            <div class="bar-card">
              <div class="callout b-severity-high">
                <div class="text">
                  <div class="text-muted">{{ $t("Store.HighRisk") }}</div>
                  <strong>{{ projectInfo.metrics.high }}</strong>
                </div>
              </div>
            </div>
            <div class="bar-card">
              <div class="callout b-severity-medium">
                <div class="text">
                  <div class="text-muted">{{ $t("Store.MediumRisk") }}</div>
                  <strong>{{ projectInfo.metrics.medium }}</strong>
                </div>
              </div>
            </div>
            <div class="bar-card">
              <div class="callout b-severity-low">
                <div class="text">
                  <div class="text-muted">{{ $t("Store.LowRisk") }}</div>
                  <strong>{{ projectInfo.metrics.low }}</strong>
                </div>
              </div>
            </div>
            <div class="bar-card">
              <div class="callout b-severity-unassigned">
                <div class="text">
                  <div class="text-muted">{{ $t("Store.Unassigned") }}</div>
                  <strong>{{ projectInfo.metrics.unassigned }}</strong>
                </div>
              </div>
            </div>
            <div class="bar-card">
              <div class="callout b-severity-info">
                <div class="text">
                  <div class="text-muted">{{ $t("Store.RiskScore") }}</div>
                  <strong>{{ projectInfo.metrics.inheritedRiskScore }}</strong>
                </div>
              </div>
            </div>
            <div class="bar-card">
              <div class="callout b-number-of-components">
                <div class="text">
                  <div class="text-muted">{{ $t("Store.NumberOfComponents") }}</div>
                  <strong>{{ projectInfo.metrics.components }}</strong>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="wrapper-com">
          <a-card :bordered="true" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
            <ChartPolicyViolationBreakdown :metrics="vulnerabilitiesData"></ChartPolicyViolationBreakdown>
          </a-card>
        </div>
      </a-tab-pane>
      <a-tab-pane key="6" :tab="$t('Store.SubsidiaryFiles')" v-if="currentFileDetial && currentFileDetial.subsidiaryFiles">
        <a-list item-layout="horizontal" :data-source="currentFileDetial.subsidiaryFiles" :pagination="currentFileDetial.subsidiaryFiles.length === 0 ? false : { pageSize: 5, total: currentFileDetial.subsidiaryFiles.length, showLessItems: true }" >
          <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
            <a slot="actions" :href="item.url" target="_blank">{{$t('Store.DownLoad')}}</a>
            <a slot="actions" v-if="deleteEnabled" @click="deleteSubsidiaryHandle(index, item)" >{{$t('Store.Delete')}}</a>
            <a-list-item-meta
              :description="item.url"
            >
              <a slot="title" :href="item.url" target="_blank">{{ item.name }}</a>
            </a-list-item-meta>
          </a-list-item>
        </a-list>
      </a-tab-pane>
    </a-tabs>

    <hr class="gradient-line" />

    <a-col
      :span="24"
      v-if="
        currentFileDetial &&
        currentFileDetial.snippets &&
        currentFileDetial.snippets.length > 0 &&
        currentFileDetial.snippets.map(e => e.code).filter(e => e).length > 0
      "
    >
      <a-card :bordered="false" class="card-billing-info">
        <div class="col-info">
          <a-descriptions
            :title="$t('Store.UseExamples') + '(' + codeParam.type + ')'"
            :column="1"
          >
            <a-descriptions-item v-if="currentFileDetial">
              <prism-editor
                class="my-editor height-300"
                v-if="currentFileDetial"
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
            v-for="(item, index) in this.currentFileDetial.snippets"
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
import { getArtifact, deleteArtifact, getStorageAndRepositoryPermission } from "@/api/folib";
import { getProjectInfo, getCacheConfig } from "@/api/foEyes";
import { deleteArtifactMetadata, conanInfo, conanPackageInfo } from "@/api/artifact";
import { getMetadataConfiguration } from '@/api/settings'
import { getProjectsVulnerabilities } from "@/api/projects.js"
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
import ChartPolicyViolationBreakdown from "../../../ComponentAnalysis/Projects/Components/ChartPolicyViolationBreakdown.vue"

export default {
  name: "BaseData",
  props: [
    "currentTreeNode",
    "repositoryType",
    "currentFileDetial",
    "folibRepository",
    "successMsg",
  ],
  components: {
    PrismEditor,
    quillEditor,
    ChartPolicyViolationBreakdown,
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
        {
          i18nKey: 'Store.Operations',
          title: "操作",
          dataIndex: "operation",
          width: 250,
          scopedSlots: { customRender: "operation" },
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
      showMetadataHandler: false,
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
      selectedColor: "#2db7f5",
      projectInfo: {
        uuid:'',
        metrics: {
          critical: 0,
          high: 0,
          medium: 0,
          low: 0,
          unassigned: 0,
          vulnerabilities: 0,
          vulnerableComponents: 0,
          components: 0,
          suppressed: 0,
          inheritedRiskScore: 0,
        }
      },
      vulnerabilitiesData: [],
      deleteEnabled: false,
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
    currentFileDetial: function (val) {
      if (val && val.snippets) {
        this.changeCodeType(val.snippets[0])
      }
      this.metadataShow()
      this.queryProjectInfo()
      this.queryStorageAndRepositoryPermission()
    },
    'currentTreeNode.artifactPath': function (newval, oldVal) {
      this.conanInfoReset()
      this.conanPackageInfoReset()
      this.metadataList = []
      if (this.currentTreeNode.type === 'file') {
        this.getMetadata()
        this.selectedTag = 0
      } else if (newval && newval.split('/').length === 5 && this.folibRepository.layout === 'conan'){
        this.conanInfoVisible = true
        this.getConanInfo()
      } else if (newval && newval.split('/').length === 8 && this.folibRepository.layout === 'conan'){
        this.conanPackageInfoVisible = true
        this.getConanPackageInfo()
      }
    },
    '$i18n.locale': function (newValue, oldValue) {
      this.metadataTypes = [
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
      ]
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
                          this.currentFileDetial &&
                          this.currentFileDetial.artifact &&
                          this.currentFileDetial.artifact.artifactFileExists
      if (!(typeof(this.metadataEnabled) == 'boolean')) {
        this.metadataEnabled = false
      }
        if(this.currentFileDetial && this.currentFileDetial.manifest && this.currentFileDetial.manifest.manifests){
            const item = this.currentFileDetial.manifest.manifests[this.selectedTag];
            this.$emit('messageArchitectureChild', item.platform.os + '/' + item.platform.architecture + (item.platform.variant? '/' + item.platform.variant : ''));
        }else {
            this.$emit('messageArchitectureChild', null);
        }
    },
    metadataHandler() {
      this.$emit("metadataHandler", 1)
    },
    artifactTabChange(activeKey) {
      this.metadataShow()
      if (activeKey === "2") {
        this.getMetadata()
      } else if (activeKey === "5") {
        this.getProjectsVulnerabilities()
      }
    },
    getMetadata() {
      if (this.currentTreeNode.type === 'file') {
        this.getMetadataConfiguration()
        getArtifact(
          this.repositoryType,
          this.currentTreeNode.storageId,
          this.currentTreeNode.repositoryId,
          this.currentTreeNode.artifactPath
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
        storageId: this.currentTreeNode.storageId,
        repositoryId: this.currentTreeNode.repositoryId,
        artifactPath: this.currentTreeNode.artifactPath,
      };
      deleteArtifactMetadata(data)
        .then((res) => {
          this.successMsg(this.$t('Store.DeletedProductSuccess'));
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
        storageId: this.currentTreeNode.storageId,
        repositoryId: this.currentTreeNode.repositoryId,
        artifactPath: this.currentTreeNode.artifactPath
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
        storageId: this.currentTreeNode.storageId,
        repositoryId: this.currentTreeNode.repositoryId,
        artifactPath: this.currentTreeNode.artifactPath
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
        this.currentTreeNode.storageId,
        this.currentTreeNode.repositoryId,
        this.currentTreeNode.artifactPath,
        item.digest
      ).then((res) => {
        this.$emit("setCurrentFileDetial", res)
        this.selectedTag = index
        this.$forceUpdate()
      })
    },
    getFoEyesEnable () {
      const cacheConfig = getCacheConfig()
      if (cacheConfig) {
        return cacheConfig.enable
      }
      return false
    },
    queryProjectInfo() {
      if (!this.getFoEyesEnable()) {
        return false
      }
      if (!this.currentFileDetial || !this.currentFileDetial.artifact) {
        return false
      }
      let artifact = this.currentFileDetial.artifact
      getProjectInfo(artifact.storageId, artifact.repositoryId, artifact.artifactPath).then((res) => {
        this.projectInfo = res
        this.getProjectsVulnerabilities()
      })
    },
    handleGoDetail() {
      const routeUrl = this.$router.resolve({path: `/projectsDetail/${this.projectInfo.uuid}`})
      window.open(routeUrl.href, '_blank')
    },
    getProjectsVulnerabilities() {
      const uuid = this.projectInfo.uuid
      if (!uuid) {
        return false
      }
      getProjectsVulnerabilities(uuid).then((res) => {
        this.vulnerabilitiesData = res.data
      })
    },
    queryStorageAndRepositoryPermission () {
      if (!this.currentFileDetial || !this.currentFileDetial.artifact) {
        return false
      }
      let artifact = this.currentFileDetial.artifact
      let permissions = []
      getStorageAndRepositoryPermission(
        artifact.storageId,
        artifact.repositoryId
      ).then(res => {
        permissions = res.permissions
        this.deleteEnabled =
          this.folibRepository.type !== 'group' &&
          (hasRole('ARTIFACTS_MANAGER') ||
          permissions.includes('ARTIFACTS_DELETE'))
      })
    },
    deleteSubsidiaryHandle(index, item) {
      if (!this.currentFileDetial || !this.currentFileDetial.artifact) {
        return false
      }
      let artifact = this.currentFileDetial.artifact
      deleteArtifact(
        artifact.storageId,
        artifact.repositoryId,
        item.path
      ).then(res => {
        this.currentFileDetial.subsidiaryFiles.splice(index, 1)
        setTimeout(() => {
          this.$notification.success({
            message: this.$t('Store.DeletionSuccessful')
          })
        }, 100)
      }).catch(err => {
        let errStatusArr = [403, 401]
        if (errStatusArr.includes(err.response.status)) {
          return false
        }
        let msg = err.response.data.message
          ? err.response.data.message
          : err.response.data.error
            ? err.response.data.error
            : err.response.data
        if (!msg || msg.length === 0 || typeof msg === 'object')
        {
          msg = this.$t('Store.DeletionFailed')
        }
        this.$notification.error({
          message: msg,
          description: ''
        })
      }).finally(() => { })
    },
  },
};
</script>

<style lang="scss" scoped>
::v-deep .part-title {
    font-size: 18px;
    font-weight: 600;
}
::v-deep .part-sub-title {
    font-size: 11px;
    color: rgba(115, 129, 143, 0.7);
}
.callout {
  height: 50px;
  position: relative;
  padding: 0 1rem;
  margin: 1rem 0;
  border-left: 4px solid #0b1015;
  border-radius: 0.25rem;
  border-left-color: #6dd9ff;
}
strong {
  font-size: 20px;
}
.bar {
  width: 100%;
  .card-inner {
    width: 100%;
    display: flex;
    justify-content: space-evenly;
  }
}
.bar {
  width: 100%;
  .card-inner {
    width: 100%;
    display: flex;
    justify-content: space-evenly;
  }
}
.bar-card {
  height: 100px;
}
.wrapper-com {
  width: 100%;
  display: block;
  .header-solid {
    margin: 10px 10px 0px 10px;
  }
}
</style>
