<template>
  <div class="wrapper component-vulnerability">
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search placeholder="输入漏洞编号查询" class="v-search" v-model="queryParams.searchKeyword" @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="columns"
        :data-source="vulnerabilityData"
        @change="handleChangeTable"
        :scroll="{ x: true }"
        :loading="vulnerabilityTableLoading"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }"
      >
        <template slot="uuid" slot-scope="uuid">
          <a-button type="link" @click="handleGoVul(uuid)">
            {{ uuid }}
          </a-button>
        </template>
        <template slot="cvssV2Severity" slot-scope="cvssV2Severity">
          <div class="table-avatar-info" v-if="cvssV2Severity">
            <a-avatar v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(cvssV2Severity) != -1" :size="24"
              :src="'images/folib/' + cvssV2Severity.toLowerCase() + '.svg'" />
            <a-avatar v-else shape="circle" :size="24">{{ cvssV2Severity.slice(0, 1) }}</a-avatar>
            <div class="avatar-info">
              <p class="mb-0 text-dark">{{
                  cvssV2Severity === 'CRITICAL' ? '严重' : cvssV2Severity === 'MEDIUM' ? '中危' : cvssV2Severity === 'HIGH' ? '高危' : cvssV2Severity === 'LOW' ? '低危' : cvssV2Severity
              }}
              </p>
            </div>
          </div>
        </template>
        <template slot="cvssV3Severity" slot-scope="cvssV3Severity">
          <div class="table-avatar-info" v-if="cvssV3Severity">
            <a-avatar v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(cvssV3Severity) != -1" :size="24"
              :src="'images/folib/' + cvssV3Severity.toLowerCase() + '.svg'" />
            <a-avatar v-else shape="circle" :size="24">{{ cvssV3Severity.slice(0, 1) }}</a-avatar>
            <div class="avatar-info">
              <p class="mb-0 text-dark">{{
                  cvssV3Severity === 'CRITICAL' ? '严重' : cvssV3Severity === 'MEDIUM' ? '中危' : cvssV3Severity === 'HIGH' ? '高危' : cvssV3Severity === 'LOW' ? '低危' : cvssV3Severity
              }}
              </p>
            </div>
          </div>
        </template>
        <template slot="expandedRowRender" slot-scope="record">
          <a-tag color="#87d068" class="description-title">漏洞描述</a-tag>
          <a-textarea class="description" :autoSize="true" :read-only="true" v-model="record.description" />
        </template>
        <template slot="highestSeverityText" slot-scope="highestSeverityText">
          <div class="table-avatar-info">
            <a-avatar v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(highestSeverityText) != -1" :size="24"
              :src="'images/folib/' + highestSeverityText.toLowerCase() + '.svg'" />
            <a-avatar v-else shape="circle" :size="24">{{ highestSeverityText.slice(0, 1) }}</a-avatar>
            <div class="avatar-info">
              <p class="mb-0 text-dark">{{
                  highestSeverityText === 'CRITICAL' ? '严重' : highestSeverityText === 'MEDIUM' ? '中危' : highestSeverityText === 'HIGH' ? '高危' : highestSeverityText === 'LOW' ? '低危' : highestSeverityText
              }}
              </p>
            </div>
          </div>
        </template>
      </a-table>
    </a-card>
  </div>
</template>

<script>
import { getVulnerability } from "@/api/module.js";
import { formatTimestamp } from "@/utils/util.js";
export default {
  props: {
    component: {
      type: Object,
      default: {},
    },
  },
  components: {},
  data() {
    return {
      columns: [
        {
          title: '漏洞编号',
          dataIndex: 'uuid',
          scopedSlots: { customRender: 'uuid' },
        },
        {
          title: '引入时间',
          dataIndex: 'created',
          scopedSlots: { customRender: 'created' },
          align: "center",
        },
        {
          title: 'CvssV2评分',
          dataIndex: 'cvssV2Score',
          scopedSlots: { customRender: 'cvssV2Score' },
          align: "center",
        },
        {
          title: 'CvssV2漏洞等级',
          dataIndex: 'cvssV2Severity',
          scopedSlots: { customRender: 'cvssV2Severity' },
          align: "center",
        },
        {
          title: 'CvssV3评分',
          dataIndex: 'cvssV3Score',
          scopedSlots: { customRender: 'cvssV3Score' },
          align: "center",
        },
        {
          title: 'CvssV3漏洞等级',
          dataIndex: 'cvssV3Severity',
          scopedSlots: { customRender: 'cvssV3Severity' },
          align: "center",
        },
        {
          title: '最高漏洞等级',
          dataIndex: 'highestSeverityText',
          scopedSlots: { customRender: 'highestSeverityText' },
          align: "center",
        },
        {
          title: '建议修复版本',
          dataIndex: 'versionEndExcluding',
          scopedSlots: { customRender: 'versionEndExcluding' },
        },
      ],
      vulnerabilityData: [],
      vulnerabilityTableLoading: false,
      queryParams: {
        page: 1,
        limit: 10,
        total: 0,
        componentUuid: "",
        searchKeyword: "",
      },
    };
  },
  watch: {
    component() {
      this.getData()
    },
    deep: true,
  },
  created() {
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      if (!this.component.uuid) {
        return
      }
      this.queryParams.componentUuid = this.component.uuid
      this.vulnerabilityTableLoading = true
      getVulnerability(this.queryParams).then((res) => {
        this.queryParams.total = res.data.total
        this.vulnerabilityData = res.data.rows
      }).finally(() => {
        this.vulnerabilityTableLoading = false
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.queryParams.page = pagination.current
      }
      this.queryParams.sortName = sorter.field
      if (sorter && sorter.order === "descend") {
        this.queryParams.sortOrder = "desc"
      } else if (sorter && sorter.order === "ascend") {
        this.queryParams.sortOrder = "asc"
      } else {
        this.queryParams.sortOrder = ""
      }
      this.getData()
    },
    handleGoCom(row) {
      this.$router.push(`/components/componentsDetail/${row.component.uuid}`)
    },
    handleGoVul(uuid) {
      this.$router.push(`/vulnerabilities/vulnerabilitiesDetail/${uuid}`)
    },
    handheTableSearch() {
      this.queryParams.page = 1
      this.getData()
    },
  },
};
</script>

<style lang="scss" scoped>
.component-vulnerability::v-deep {
  .search {
    height: 50px;
  }
  .mx-25 .ant-row-flex {
    flex-wrap: wrap;
  }
  .v-search {
    max-width: 200px;
    width: 170px;
    min-width: 150px;
    margin-left: 5px;
    margin-bottom: 8px;
  }

  .table-avatar-info .ant-avatar {
    margin-right: 8px;
  }

  .description {
    width: 92%;
    border: none;
    box-shadow: none;
    resize: none;
    background: #fbfbfb;
    vertical-align: middle;
  }

  .description-title {
    vertical-align: middle;
  }
}
</style>
