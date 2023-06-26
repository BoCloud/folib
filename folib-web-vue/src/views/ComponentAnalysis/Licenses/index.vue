<template>
  <div class="wrapper">
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search placeholder="输入许可证名称查询" class="v-search" v-model="queryParams.searchKeyword" @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="columns2"
        :data-source="licenseData"
        @change="handleChangeTable"
        :scroll="{ x: true }"
        :loading="licenseTableLoading"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }"
      >
        <template slot="licenseId" slot-scope="licenseId, row">
          <a-button type="link" @click="handleGoDetail(row)">
            {{ licenseId }}
          </a-button>
        </template>
      </a-table>
    </a-card>
  </div>
</template>

<script>
import { getLicensesList } from "@/api/licenses.js";
import { formatTimestamp } from "@/utils/util.js";
export default {
  components: {  },
  data() {
    return {
      columns2: [
        {
          title: "名称",
          dataIndex: "licenseName",
        },
        {
          title: "许可证编号",
          dataIndex: "licenseId",
          scopedSlots: { customRender: "licenseId" },
        },
      ],
      licenseData: [],
      licenseTableLoading: false,
      queryParams: {
        page: 1,
        limit: 10,
        total: 0,
        searchKeyword: ''
      },
    };
  },
  created() {
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      this.licenseTableLoading = true
      getLicensesList(this.queryParams).then((res) => {
        this.queryParams.total = res.data.total
        this.licenseData = res.data.rows
      }).finally(() => {
        this.licenseTableLoading = false
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
      this.$router.push(`/licensesDetail/${row.licenseId}`)
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
