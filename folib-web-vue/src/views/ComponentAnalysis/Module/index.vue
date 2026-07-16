<template>
  <div class="wrapper">
    <a-card :bordered="false" style="margin-top: 10px; margin-bottom: 20px; overflow-y: auto">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search :placeholder="$t('Module.groupNameQuery')" class="v-search" v-model="queryParams.groupId" @search="handheTableSearch()" />
          <a-input-search :placeholder="$t('Module.componentNameQuery')" class="v-search" v-model="queryParams.name" @search="handheTableSearch()" />
          <a-input-search :placeholder="$t('Module.versionQuery')" class="v-search" v-model="queryParams.version" @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="i18nColumns"
        :data-source="compentsData"
        @change="handleChangeTable"
        :scroll="{ x: true }"
        :loading="compentsTableLoading"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }"
      >
        <template slot="name" slot-scope="name, row">
          <a-button type="link" @click="handleGoDetail(row)">
            {{ name }}
          </a-button>
        </template>
        <template slot="license" slot-scope="license, row">
          <span v-for="(item, index) in row.license" :key="index" class="mr-5">
            <a-button type="link" @click="handleGoLicense(item)">
              {{ item }}
            </a-button>
          </span>
        </template>
        <template slot="vulnerabilitiesCount" slot-scope="vulnerabilitiesCount, row">
          <a-tag color="#f86c6b">{{ row.criticalVulnerabilitiesCount }} </a-tag>
          <a-tag color="#fd8c00">{{ row.highVulnerabilitiesCount }} </a-tag>
          <a-tag color="#ffc107">{{ row.mediumVulnerabilitiesCount }} </a-tag>
          <a-tag color="#4dbd74"> {{ row.lowVulnerabilitiesCount}}</a-tag>
        </template>
      </a-table>
    </a-card>
  </div>
</template>

<script>
import { getComponentsList } from "@/api/module.js";
import { formatTimestamp } from "@/utils/util.js";
export default {
  components: {  },
  data() {
    return {
      columns: [
        {
          title: "组件名称",
          i18nKey: 'Module.ComponentName',
          dataIndex: "name",
          scopedSlots: { customRender: "name" },
        },
        {
          title: "版本",
          i18nKey: 'Module.VersionNumber',
          dataIndex: "version",
          width: "150px",
        },
        {
          title: "组",
          i18nKey: 'Module.Group',
          dataIndex: "groupId",
          scopedSlots: { customRender: "groupId" },
        },
        {
          title: "许可证",
          i18nKey: 'Module.Licence',
          dataIndex: "license",
          scopedSlots: { customRender: "license" },
          width: "200px",
        },
        {
          title: "PURL",
          dataIndex: "purl",
          scopedSlots: { customRender: "purl" },
          width: "500px",
        },
        {
          title: "漏洞",
          i18nKey: 'Module.Vulnerability',
          dataIndex: "vulnerabilitiesCount",
          scopedSlots: { customRender: "vulnerabilitiesCount" },
          width: "200px",
        },
      ],
      compentsData: [],
      compentsTableLoading: false,
      queryParams: {
        page: 1,
        limit: 10,
        sortOrder: "",
        sortName: "",
        groupId: "",
        name: "",
        version: "",
        total: 0,
      },
    };
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
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      this.compentsTableLoading = true
      getComponentsList(this.queryParams).then((res) => {
        this.queryParams.total = res.data.total
        this.compentsData = res.data.rows
      }).finally(() => {
        this.compentsTableLoading = false
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
      this.$router.push(`/components/componentsDetail/${row.uuid}`)
    },
    handleGoProject(row) {
      this.$router.push(`/artifacts/artifactsDetail/${row.project.uuid}`)
    },
    handleGoLicense(item) {
      this.$router.push(`/licenses/licensesDetail/${item}`)
    },
    handheTableSearch() {
      this.queryParams.page = 1
      this.getData()
    },
  },
};
</script>

<style lang="scss" scoped>
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
</style>
