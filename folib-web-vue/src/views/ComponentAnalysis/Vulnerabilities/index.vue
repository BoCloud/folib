<template>
  <div class="wrapper vulnerability-database">
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search :placeholder="$t('Vulnerabilities.EnterVulnerabilityNumber')" class="v-search" v-model="queryParams.searchKeyword"
            @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table rowKey="uuid" class="mt-20" :columns="i18nColumns2" :data-source="vulnerabilityDatabaseData"
        @change="handleChangeTable" :scroll="{ x: true }" :loading="vulnerabilityTableLoading"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }">
        <template slot="cve" slot-scope="cve, row">
          <a-button type="link" @click="handleGoDetail(row)">
            {{ cve }}
          </a-button>
        </template>
        <template slot="cweList" slot-scope="cweList">
          {{ cweList?cweList.join(","):'' }}
        </template>
        <template slot="severity" slot-scope="severity">
          <div class="table-avatar-info" v-if="severity">
            <a-avatar v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(severity) != -1" :size="24"
              :src="'images/folib/' + severity.toLowerCase() + '.svg'" />
            <a-avatar v-else shape="circle" :size="24">{{ severity.slice(0, 1) }}</a-avatar>
            <div class="avatar-info">
              <p class="mb-0 text-dark">
                {{
                  severity === "CRITICAL"
                  ? $t('Vulnerabilities.Seriously')
                  : severity === "MEDIUM"
                    ? $t('Vulnerabilities.MediumRisk')
                    : severity === "HIGH"
                      ? $t('Vulnerabilities.HighRisk')
                      : severity === "LOW"
                        ? $t('Vulnerabilities.LowRisk')
                        : $t('Vulnerabilities.Unassigned')
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
import { getVulnerabilitiesList } from "@/api/vulnerabilities.js";
import { formatTimestamp } from "@/utils/util.js";
export default {
  components: {},
  data() {
    return {
      columns2: [
        {
          title: "漏洞编号",
          i18nKey: 'Vulnerabilities.VulnerabilityNumber',
          dataIndex: "cve",
          scopedSlots: { customRender: "cve" },
        },
        {
          title: "CVSS评分",
          i18nKey: 'Vulnerabilities.CVSSScore',
          dataIndex: "cvssScore",
          scopedSlots: { customRender: "cvssScore" },
        },
        {

          title: "漏洞类型",
          i18nKey: 'Vulnerabilities.VulnerabilityType',
          dataIndex: "cweList",
          scopedSlots: { customRender: "cweList" },
        },
        {
          title: "受影响的制品",
          i18nKey: 'Vulnerabilities.AffectedProducts',
          dataIndex: "artifactCount",
          scopedSlots: { customRender: "artifactCount" },
        },
        {
          title: "等级",
          i18nKey: 'Vulnerabilities.Level',
          dataIndex: "severity",
          scopedSlots: { customRender: "severity" },
        },
      ],
      vulnerabilityDatabaseData: [],
      vulnerabilityTableLoading: false,
      queryParams: {
        page: 1,
        limit: 10,
        searchKeyword: "",
        sortOrder: "",
        sortName: "",
        total: 0,
      },
    };
  },
  computed: {
    i18nColumns2() {
      return this.columns2.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  created() {
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      this.vulnerabilityTableLoading = true
      getVulnerabilitiesList(this.queryParams).then((res) => {
        this.queryParams.total = res.data.total
        this.vulnerabilityDatabaseData = res.data.rows
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
    handleGoDetail(row) {
      this.$router.push(`/vulnerabilities/vulnerabilitiesDetail/${row.cve}`)
    },
    handheTableSearch() {
      this.queryParams.page = 1
      this.getData()
    },
  },
};
</script>

<style lang="scss" scoped>
.vulnerability-database::v-deep {
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
}
</style>
