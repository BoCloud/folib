<template>
  <div>
    <a-col :span="24" :md="24" class="mb-24">
      <a-card :bordered="false" style="max-height: 1024px; min-height: 454px; overflow-y: auto" class="header-solid"
        :bodyStyle="{ paddingTop: 0, paddingBottom: 0 }">
        <div class="mx-25">
          <a-row type="flex" :gutter="24">
            <a-col :span="24" md="12">
              <label for="" class="ml-10">显示数量</label>
              <a-select class="ml-10 mt-10" v-model="artifactQuery.limit" @change="onPageSizeChange" style="width: 70px">
                <a-select-option :value="5">5</a-select-option>
                <a-select-option :value="10">10</a-select-option>
                <a-select-option :value="15">15</a-select-option>
                <a-select-option :value="20">20</a-select-option>
                <a-select-option :value="25">25</a-select-option>
              </a-select>
              <a-config-provider class="ml-10 mt-10" :locale="locale" style="width: 290px">
                <a-range-picker :show-time="{ placeholder: '选择时间', format: 'HH:mm' }" format="YYYY-MM-DD HH:mm"
                  :placeholder="['开始日期', '结束日期']" @change="dateChange" @ok="dateConfirm" />
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
            <span style="vertical-align: super;">搜索列表</span>
          </h6>
        </template>
        <a-table class="mt-20" :columns="columns" rowKey="url" :data-source="searchData" :scroll="{ x: true }" @change="handleTableChange"
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

    <a-drawer placement="right" width="75%" title="制品详情" :visible="artifactVisible" @close="artifactVisible = false"
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
        <a class="text-dark" :href="searchDataCurrentSelect ? searchDataCurrentSelect.url : ''" target="_blank">{{
          searchDataCurrentSelect ? searchDataCurrentSelect.url : "" }}</a>
        <hr class="my-25" />
        <a-descriptions title="基本信息" :column="1" v-if="searchDataCurrentSelect">
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
          <template v-if="searchDataCurrentSelect && searchDataCurrentSelect.checksums">
            <a-descriptions-item :label="key" v-for="(value, key, index) in searchDataCurrentSelect.checksums"
              :key="index">
              {{ value }}
            </a-descriptions-item>
          </template>
        </a-descriptions>
        <hr class="my-25" />

        <a-col :span="24" v-if="
          searchDataCurrentSelect &&
          searchDataCurrentSelect.snippets &&
          searchDataCurrentSelect.snippets.length > 0
        ">
          <a-card :bordered="false" class="card-billing-info">
            <div class="col-info">
              <a-descriptions :title="'使用示例(' + codeParam.type + ')'" :column="1">
                <a-descriptions-item v-if="searchDataCurrentSelect">
                  <prism-editor class="my-editor height-300" v-if="searchDataCurrentSelect" v-model="codeParam.code"
                    :highlight="highlighterHandle" :line-numbers="false" :readonly="true"></prism-editor>
                </a-descriptions-item>
              </a-descriptions>
            </div>
            <div class="col-action">
              <a-button v-for="(item, index) in this.searchDataCurrentSelect.snippets" :key="index" type="link"
                size="small" @click="changeCodeTye(item)">
                <a-avatar :size="20" shape="square" :src="'images/folib/' + getCodeImg(item) + '.svg'" />
              </a-button>
            </div>
          </a-card>
        </a-col>
      </a-card>
    </a-drawer>

    <!-- 搜索预览 -->
    <a-drawer placement="right" width="45%" v-if="searchDataCurrentSelect" :title="searchDataCurrentSelect.artifactPath"
      :visible="searchViewCodeVisible" @close="closeSearchviewCodeDialog">
      <div class="mx-auto m-50" style="max-width: 1000px">
        <div class="mb-50">
          <a-card :bordered="false" class="header-solid">
            <a-directory-tree v-if="searchDataCurrentSelect && searchDataCurrentSelect.treeNode"
              :replaceFields="{ title: 'name', children: 'children' }" :tree-data="searchDataCurrentSelect.treeNode" />
          </a-card>
          <prism-editor class="my-editor height-300" v-if="searchDataCurrentSelect && searchViewCodes"
            v-model="searchViewCodes" :highlight="highlighterHandle" :line-numbers="false" :readonly="true">
          </prism-editor>
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
  viewArtifactFile,
} from "@/api/folib"
import { hasRole, isAdmin, hasPermission, isLogin } from "@/utils/permission"
import VunlerabilityReport from '@/components/Vulnerabilities/VunlerabilityReport'

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
    openRepository: {
      type: Boolean,
      default: false,
    },
    columns: {
      type: Array,
      default: [],
    },
  },
  components: {
    PrismEditor,
    VunlerabilityReport
  },
  data() {
    return {
      locale: zhCN,
      loading: false,
      artifactQuery: {
        artifactName: null,
        metadataSearch: null,
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
      artifactVisible: false,
      reportVisible: false,
    }
  },
  created() {

  },
  mounted() { },
  methods: {
    message(type, message) {
      if (!message) {
        message = "操作成功"
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
        };
      }
    },
    getCodeImg(item) {
      return item.name === "Maven 2" ? "maven_black" : item.name.toLowerCase()
    },
    searchDataHandle(item) {
      this.searchDataCurrentSelect = item
      if (
        this.searchDataCurrentSelect &&
        this.searchDataCurrentSelect.snippets
      ) {
        this.changeCodeTye(this.searchDataCurrentSelect.snippets[0])
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
        item.layout,
        item.storageId,
        item.repositoryId,
        item.artifactPath
      ).then((res) => {
        let artifact = res.artifact
        if (isLogin() && artifact && artifact.safeLevel === "scanComplete") {
          this.scanReport.show = true
          this.scanReport.vulnerabilitesCount = artifact.vulnerabilitiesCount
          this.scanReport.critical = artifact.criticalVulnerabilitiesCount
          this.scanReport.high = artifact.highVulnerabilitiesCount
          this.scanReport.medium = artifact.mediumVulnerabilitiesCount
          this.scanReport.low = artifact.lowVulnerabilitiesCount
          this.scanReport.report = JSON.parse(artifact.report)
        }
      });
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
        } else if (this.searchType === 2) {
          this.artifactQuery.metadataSearch = searchValue
          this.artifactQuery.artifactName = null
        }
      }
      this.artifactQuery.storageId = this.folibRepository.storageId
      this.artifactQuery.repositoryId = this.folibRepository.id
      let params = {
        artifactName: this.artifactQuery.artifactName,
        metadataSearch: this.artifactQuery.metadataSearch,
        storageId: this.artifactQuery.storageId,
        repositoryId: this.artifactQuery.repositoryId,
        limit: this.artifactQuery.limit,
        page: this.artifactQuery.page,
        sortField: this.artifactQuery.sortField,
        sortOrder: this.artifactQuery.sortOrder,
        beginDate: this.artifactQuery.beginDate,
        endDate: this.artifactQuery.endDate,
        regex: false,
        openRepository: this.openRepository
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
      if (
        this.searchDataCurrentSelect &&
        !this.searchDataCurrentSelect.treeNode
      ) {
        viewArtifactFile(this.searchDataCurrentSelect.url).then((res) => {
          if ("string" === typeof res && res.startsWith("PK")) {
            this.searchViewCodes = undefined;
          } else if ("object" === typeof res) {
            this.searchViewCodes = JSON.stringify(res)
          } else {
            this.searchViewCodes = res
          }
        });
      }
      this.searchViewCodeVisible = true
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
  }
}
</script>