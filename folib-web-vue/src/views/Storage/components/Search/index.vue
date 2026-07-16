<template>
  <div>
    <a-col :span="24" :md="24" class="mb-24">
      <a-card :bordered="false" style="max-height: 1024px; min-height: 454px; overflow-y: auto" class="header-solid"
        :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }">
        <div class="mx-25">
          <a-row type="flex" :gutter="24">
            <a-col :span="24" md="12">
              <label for="" class="ml-10">{{ $t('Store.DisplayQuantity') }}</label>
              <a-select class="ml-10 mt-10" v-model="artifactQuery.limit" @change="onPageSizeChange" style="width: 70px">
                <a-select-option :value="5">5</a-select-option>
                <a-select-option :value="10">10</a-select-option>
                <a-select-option :value="15">15</a-select-option>
                <a-select-option :value="20">20</a-select-option>
                <a-select-option :value="25">25</a-select-option>
              </a-select>
              <a-config-provider class="ml-10 mt-10" :locale="locale" style="width: 290px">
                <a-range-picker :show-time="{ placeholder: $t('Store.SelectDate'), format: 'HH:mm' }" format="YYYY-MM-DD HH:mm"
                  :placeholder="[$t('Store.StartDate'), $t('Store.EndDate')]" @change="dateChange" @ok="dateConfirm" />
              </a-config-provider>
            </a-col>
            <a-col :span="24" md="12"> </a-col>
          </a-row>
        </div>
        <template #title>
          <h6 class="font-semibold m-0">
            <a>
              <a-icon type="backward" :style="{
                fontSize: '32px',
                marginRight: '5px',
                opacity: '0.8',
                color: '#BFBFBFFF',
              }" @click="goBack()" />
            </a>
            <span style="vertical-align: super;">{{ $t('Store.SearchList') }}</span>
          </h6>
        </template>
        <a-table class="mt-20" :columns="i18nColumns" rowKey="url" :data-source="searchData" :scroll="{ x: true }" @change="handleTableChange"
          :loading="loading" :pagination="{
            pageSize: artifactQuery.limit,
            current: artifactQuery.page,
            total: artifactQuery.total,
            showLessItems: true,
          }">
          <template slot="path" slot-scope="text, record">
            <a>
              <div class="table-avatar-info" @click="searchDataHandle(record)">
                <a-avatar shape="circle" :size="24" :src="
                  'images/folib/' + getFileImage(record.layout, record.path) + '.svg'
                " />
                <div class="avatar-info search-column-path">
                  <p class="mb-0 text-dark">
                    {{ record.artifactPath }}
                  </p>
                </div>
              </div>
            </a>
          </template>
          <template slot="sizeInBytes" slot-scope="sizeInBytes">{{
            fileSizeConver(sizeInBytes)
          }}</template>
        </a-table>
      </a-card>
    </a-col>

    <a-drawer placement="right" width="65%" :title="$t('Store.ProductDetails')" :visible="artifactVisible" @close="artifactVisible = false"
      :zIndex="100">
      <a-card :bordered="false" class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
        <template #title>
          <h6 class="font-semibold m-0">
            <a-avatar :size="24" shape="square" :src="
              'images/folib/' +
              getFileImage(
                searchDataCurrentSelect.layout,
                searchDataCurrentSelect
                  ? searchDataCurrentSelect.path
                  : ''
              ) +
              '.svg'
            " />
            {{ searchDataCurrentSelect ? searchDataCurrentSelect.artifactPath : "" }}
            <span class="ml-auto" v-if="scanReport.show" @click="reportVisible = true">
              <a-space :size="1" class="avatar-chips">
                <template v-if="scanReport.vulnerabilitesCount > 0">
                  <a-tooltip>
                    <template slot="title">{{ $t('Store.Seriousness') }}</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/critical.svg'" />
                      <span class="mb-0 text-dark">{{
                        scanReport.critical
                      }}</span>
                    </div>
                  </a-tooltip>

                  <a-tooltip>
                    <template slot="title">{{ $t('Store.HighRisk') }}</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/high.svg'" />
                      <span class="mb-0 text-dark">{{ scanReport.high }}</span>
                    </div>
                  </a-tooltip>

                  <a-tooltip>
                    <template slot="title">{{ $t('Store.MediumRisk') }}</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/medium.svg'" />
                      <span class="mb-0 text-dark">{{
                        scanReport.medium
                      }}</span>
                    </div>
                  </a-tooltip>

                  <a-tooltip>
                    <template slot="title">{{ $t('Store.LowRisk') }}</template>
                    <div class="">
                      <a-avatar :size="24" :src="'images/folib/low.svg'" />
                      <span class="mb-0 text-dark">{{ scanReport.low }}</span>
                    </div>
                  </a-tooltip>
                </template>
                <template v-else>
                  <a-tooltip>
                    <template slot="title">{{ $t('Store.Health') }}</template>
                    <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
                  </a-tooltip>
                </template>
              </a-space>
            </span>
          </h6>
        </template>
        <a-button type="link" slot="extra" @click="searchViewCodeHandle()">
          {{ $t('Store.Preview') }}
          <a-icon :size="24" shape="square" type="eye"></a-icon>
        </a-button>
        <a class="text-dark" v-if="searchDataCurrentSelect.layout !== 'Docker'" :href="searchDataCurrentSelect ? searchDataCurrentSelect.url : ''" target="_blank" rel="noopenner noreferrer">{{
          searchDataCurrentSelect ? searchDataCurrentSelect.url : "" }}</a>
        <hr class="my-25" />
        <ArtifactData ref="artifactData" v-if="artifactVisible && searchDataCurrentSelectItem" :artifactName="searchDataCurrentSelect.artifactName" :artifactPath="searchDataCurrentSelect.artifactPath" :currentArtifact="searchDataCurrentSelectItem" :repositoryType="searchDataCurrentSelect.layout" :artifact="searchDataCurrentSelectItem.artifact"
        :folibRepository="folibRepository" @message="message" @setCurrentArtifact="setCurrentArtifact" />
      </a-card>
    </a-drawer>

    <!-- 搜索预览 -->
    <a-drawer placement="right" width="45%" v-if="searchDataCurrentSelectItem" :title="searchDataCurrentSelectItem.artifactPath"
      :visible="searchViewCodeVisible" @close="closeSearchviewCodeDialog">
      <div class="mx-auto m-50" style="max-width: 1000px">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree v-if="searchDataCurrentSelectItem && searchDataCurrentSelectItem.listTree"
              :replaceFields="{ title: 'name', children: 'children' }" :tree-data="searchDataCurrentSelectItem.listTree" />
          </a-card>
          <prism-editor class="my-editor height-300" v-if="searchDataCurrentSelectItem && searchViewCodes"
            v-model="searchViewCodes" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>

          <a-tabs v-if="searchDataCurrentSelectItem &&
            searchDataCurrentSelectItem.manifestConfig
            " class="tabs-sliding" default-active-key="1">
            <a-tab-pane key="1" tab="Layers">
              <a-timeline>
                <a-timeline-item color="primary" v-for="(key, index) in searchDataCurrentSelectItem.manifestConfig.config" :key="index">
                  {{ index }}
                  <p>
                    {{ searchDataCurrentSelectItem.manifestConfig.config[index] }}
                  </p>
                </a-timeline-item>
              </a-timeline>
            </a-tab-pane>
            <a-tab-pane key="2" :tab="$t('Store.ProductionHistory')">
              <a-timeline>
                <a-timeline-item color="primary" v-for="(key, index) in searchDataCurrentSelectItem.manifestConfig.history" :key="index">
                  {{ formateDate(key.created) }}
                  <p>
                    {{ key.created_by }}
                  </p>
                </a-timeline-item>
              </a-timeline>
            </a-tab-pane>
          </a-tabs>
        </div>
      </div>
    </a-drawer>

    <VunlerabilityReport :report="scanReport.report" :reportVisible="reportVisible" @closeReport="closeReport" />

  </div>
</template>
<script>
import zhCN from "ant-design-vue/es/locale/zh_CN"
import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css"
import { highlight, languages } from "prismjs/components/prism-core"
import "prismjs/components/prism-clike"
import "prismjs/components/prism-javascript"
import "prismjs/themes/prism-tomorrow.css"
import {
  getFileImage,
  fileSizeConver,
} from "@/utils/layoutUtil"
import {
  fql,
  getArtifact,
  getArtifactReport,
  previewArtifact,
  viewArtifactFile,
} from "@/api/folib"
import { hasRole, isAdmin, hasPermission, isLogin } from "@/utils/permission"
import VunlerabilityReport from '@/components/Vulnerabilities/VunlerabilityReport'
import ArtifactData from './Data.vue'

export default {
  inject: ["reload"],
  props: {
    folibRepository: {
      type: Object,
      default: () => {
        return {
          id: '',
          layout: '',
          storageId: '',
        }
      }
    },
    columns: {
      type: Array,
      default: [],
    },
  },
  components: {
    PrismEditor,
    VunlerabilityReport,
    ArtifactData
  },
  data() {
    return {
      locale: zhCN,
      loading: false,
      artifactQuery: {
        artifactName: null,
        metadataSearch: null,
        digest: null,
        storageId: null,
        repositoryId: null,
        limit: 5,
        page: 1,
        total: 0,
        sortField: null,
        sortOrder: null,
        beginDate: null,
        endDate: null,
      },
      searchType: 1,
      searchData: [],
      searchDataCurrentSelect: {},
      searchViewCodeVisible: false,
      searchViewCodes: null,
      scanReport: {
        show: false,
        report: [],
        vulnerabilitesCount: 0,
        critical: 0,
        high: 0,
        medium: 0,
        low: 0,
      },
      searchDataCurrentSelectItem: {},
      artifactVisible: false,
      reportVisible: false,
    }
  },
  computed: {
    i18nColumns() {
      return this.columns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  created() {

  },
  mounted() { },
  methods: {
    message(type, message) {
      if (!message) {
        message = this.$t('Store.OperationSuccess')
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
    getFileImage(layout, name) {
      if (name) {
        return getFileImage(layout, name)
      }
    },
    fileSizeConver(size) {
      if (size) {
        return fileSizeConver(size)
      }
    },
    highlighterHandle(code) {
      return highlight(code, languages.js)
    },
    dateChange(value, dateString) {
      if (dateString) {
        this.artifactQuery.beginDate = dateString[0]
        this.artifactQuery.endDate = dateString[1]
        if (
          this.artifactQuery.beginDate === "" &&
          this.artifactQuery.endDate === ""
        ) {
          this.dateConfirm()
        }
      }
    },
    dateConfirm() {
      this.search(this.artifactQuery.artifactName, null, 1)
    },
    changeCodeTye(item) {
      if (item) {
        this.codeParam = {
          type: item.name === "Maven 2" ? "maven" : item.name.toLowerCase(),
          code: item.code,
        }
        this.$forceUpdate()
      }
    },
    getCodeImg(item) {
      return item.name === "Maven 2" ? "maven" : item.name.toLowerCase()
    },
    searchDataHandle(item) {
      this.searchDataCurrentSelect = item
      this.searchDataCurrentSelectItem = {}
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
        item.layout,
        item.storageId,
        item.repositoryId,
        item.artifactPath
      ).then((res) => {
        let artifact = res.artifact
        this.searchDataCurrentSelectItem = res
        if (
          this.searchDataCurrentSelectItem &&
          this.searchDataCurrentSelectItem.snippets
        ) {
          this.changeCodeTye(this.searchDataCurrentSelectItem.snippets[0])
        }
        if (isLogin() && artifact && artifact.safeLevel === "scanComplete") {
          this.scanReport.show = true
          this.scanReport.vulnerabilitesCount = artifact.vulnerabilitiesCount
          this.scanReport.critical = artifact.criticalVulnerabilitiesCount
          this.scanReport.high = artifact.highVulnerabilitiesCount
          this.scanReport.medium = artifact.mediumVulnerabilitiesCount
          this.scanReport.low = artifact.lowVulnerabilitiesCount
          getArtifactReport(
            item.layout,
            item.storageId,
            item.repositoryId,
            item.artifactPath
          ).then(res => { 
            if (res.artifact && res.artifact.safeLevel === "scanComplete")
            {
              this.scanReport.report = JSON.parse(res.artifact.report)
            }
          })
        }
      })
      this.artifactVisible = true
    },
    handleTableChange(pagination, filters, sorter) {
      this.artifactQuery.sortField = null;
      this.artifactQuery.sortOrder = null;
      if (pagination) {
        this.artifactQuery.page = pagination.current;
      }
      if (sorter) {
        this.artifactQuery.sortField = sorter.field;
        if (sorter.order) {
          this.artifactQuery.sortOrder = "asc"
          if (sorter.order.indexOf("desc") !== -1) {
            this.artifactQuery.sortOrder = "desc"
          }
        }
      }
      this.search(this.artifactQuery.artifactName)
    },
    search(searchValue, type, page) {
      if (type) {
        this.searchType = type
      }
      if (page) {
        this.artifactQuery.page = page
      }
      if (searchValue) {
        if (this.searchType === 1) {
          this.artifactQuery.artifactName = searchValue
          this.artifactQuery.metadataSearch = null
          this.artifactQuery.digest = null
        } else if (this.searchType === 2) {
          this.artifactQuery.metadataSearch = searchValue
          this.artifactQuery.artifactName = null
          this.artifactQuery.digest = null
        } else if (this.searchType === 3) {
          this.artifactQuery.digest = searchValue
          this.artifactQuery.artifactName = null
          this.artifactQuery.metadataSearch = null
        }
      }
      this.artifactQuery.storageId = this.folibRepository.storageId
      this.artifactQuery.repositoryId = this.folibRepository.id
      let params = {
        query: this.artifactQuery.artifactName,
        metadataSearch: this.artifactQuery.metadataSearch,
        digest: this.artifactQuery.digest,
        storageId: this.artifactQuery.storageId,
        repositoryId: this.artifactQuery.repositoryId,
        limit: this.artifactQuery.limit,
        page: this.artifactQuery.page,
        sortField: this.artifactQuery.sortField,
        sortOrder: this.artifactQuery.sortOrder,
        beginDate: this.artifactQuery.beginDate,
        endDate: this.artifactQuery.endDate,
        regex: false,
      }
      this.loading = true
      fql(params).then((res) => {
        this.searchData = res.artifact
        this.artifactQuery.total = res.total
      }).finally(() => {
        this.loading = false
      })
    },
    onPageSizeChange() {
      this.search(this.artifactQuery.artifactName, null, 1)
    },
    searchViewCodeHandle() {
      if (this.searchDataCurrentSelect.layout !== 'Docker' && this.searchDataCurrentSelectItem && !this.searchDataCurrentSelectItem.listTree)
        { 
          if (this.searchDataCurrentSelectItem.artifact) {
            previewArtifact(this.searchDataCurrentSelect.storageId, this.searchDataCurrentSelect.repositoryId,this.searchDataCurrentSelect.artifactPath).then(res => {
              if (res && res.length > 0) {
                this.searchDataCurrentSelectItem.listTree = res
                this.$forceUpdate()
              } else {
                let len = this.searchDataCurrentSelectItem.artifact.sizeInBytes
                if (len && len > 1048576) {
                  this.searchViewCodes = this.$t('Store.CannotPreview')
                } else{
                  this.viewArtifactFile()
                }
              }
            })
          } else {
            this.viewArtifactFile()
          }
      }
      this.searchViewCodeVisible = true
    },
    viewArtifactFile () {
    viewArtifactFile(this.searchDataCurrentSelect.url).then(res => {
      if ('string' === typeof res && res.startsWith('PK'))
      {
        this.searchViewCodes = undefined
      } else if ('object' === typeof res)
      {
        if (res.data)
        {
          if ('string' === typeof res.data)
          {
            if (res.data.startsWith('PK')) {
              this.searchViewCodes = this.$t('Store.CannotPreview')
            } else {
              this.searchViewCodes = res.data
            }
          } else
          {
            this.searchViewCodes = JSON.stringify(res.data)
          }
        } else
        {
          this.searchViewCodes = JSON.stringify(res)
        }
      } else
      {
        this.searchViewCodes = res
      }
    })
  },
    closeSearchviewCodeDialog() {
      this.searchViewCodeVisible = false
      this.searchViewCodes = null
    },
    closeReport() {
      this.reportVisible = false
    },
    goBack() {
      this.reload()
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
    setCurrentArtifact(currentArtifact) {
      if (currentArtifact) {
        this.searchDataCurrentSelectItem = currentArtifact
        this.$forceUpdate()
      }
    },
  }
}
</script>
