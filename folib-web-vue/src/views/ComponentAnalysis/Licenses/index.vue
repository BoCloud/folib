<template>
  <div class="wrapper">
    <a-card :bordered="false" style="margin-top: 10px; margin-bottom: 20px">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search :placeholder="$t('Licenses.EnterLicenseNameQuery')" class="v-search" v-model="queryParams.searchKeyword" @search="handheTableSearch()" />
          <a-select v-model="queryParams.blackWhiteType" :placeholder="$t('黑白名单类型')" allowClear class="ml-10 black-white-type-select" @change="handheTableSearch()">
            <a-select-option :value="1">{{ $t('License白名单') }}</a-select-option>
            <a-select-option :value="2">{{ $t('License黑名单') }}</a-select-option>
          </a-select>
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="i18nColumns2"
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
        <div slot="operation" slot-scope="text, record">
          <div class="col-action">
            <a-tooltip placement="topLeft">
              <template slot="title">
                {{ $t('Vulnerabilities.TheWhiteList') }}
              </template>
              <a-popconfirm :title="$t('Vulnerabilities.SureAddedWhitelist')" okType="danger" :ok-text="$t('Vulnerabilities.BeSure')" :cancel-text="$t('Vulnerabilities.Cancel')"
                            @confirm="addBlackWhite(record, 'WHITES')"
                            v-if="operatorEnabled && record.blackWhiteType == 0">
                <div class="o-btn">
                  <img src="images/folib/white.svg" />
                </div>
              </a-popconfirm>
              <a-popconfirm :title="$t('Vulnerabilities.SureRemovedWhitelist')" okType="danger" :ok-text="$t('Vulnerabilities.BeSure')" :cancel-text="$t('Vulnerabilities.Cancel')"
                            @confirm="removeBlackWhite(record, 'WHITES')"
                            v-if="operatorEnabled && record.blackWhiteType == 1">
                <div class="o-btn o-rm">
                  <img src="images/folib/white.svg" />
                </div>
              </a-popconfirm>
            </a-tooltip>
            <a-tooltip placement="topLeft">
              <template slot="title">
                {{ $t('Vulnerabilities.Blacklist') }}
              </template>
              <a-popconfirm :title="$t('Vulnerabilities.SureAddedBlacklisted')" okType="danger" :ok-text="$t('Vulnerabilities.BeSure')" :cancel-text="$t('Vulnerabilities.Cancel')"
                            @confirm="addBlackWhite(record, 'BLACKLIST')"
                            v-if="operatorEnabled && record.blackWhiteType == 0">
                <div class="o-btn o-black">
                  <img src="images/folib/black.svg" />
                </div>
              </a-popconfirm>
              <a-popconfirm :title="$t('Vulnerabilities.SureRemovedBlacklisted')" okType="danger" :ok-text="$t('Vulnerabilities.BeSure')" :cancel-text="$t('Vulnerabilities.Cancel')"
                            @confirm="removeBlackWhite(record, 'BLACKLIST')"
                            v-if="operatorEnabled && record.blackWhiteType == 2">
                <div class="o-btn o-rm">
                  <img src="images/folib/black.svg" />
                </div>
              </a-popconfirm>
            </a-tooltip>
          </div>
        </div>
      </a-table>
    </a-card>
  </div>
</template>

<script>
import { hasRole, isAdmin, hasPermission } from "@/utils/permission";
import { getLicensesList, blackWhite } from "@/api/licenses.js";
import { formatTimestamp } from "@/utils/util.js";
import {addAllowlistDenylistBlock, deleteAllowlistDenylistBlock} from "@/api/AllowlistDenylistBlock";
import moment from "moment";
export default {
  components: {  },
  data() {
    return {
      columns2: [
        {
          title: "名称",
          i18nKey: 'Licenses.Name',
          dataIndex: "licenseName",
        },
        {
          title: "许可证编号",
          i18nKey: 'Licenses.LicenseNumber',
          dataIndex: "licenseId",
          scopedSlots: { customRender: "licenseId" },
        },
        {
          title: '操作',
          i18nKey: 'Setting.Operation',
          dataIndex: 'operation',
          width: 180,
          scopedSlots: { customRender: 'operation' },
        },
      ],
      licenseData: [],
      licenseTableLoading: false,
      queryParams: {
        page: 1,
        limit: 10,
        total: 0,
        searchKeyword: '',
        blackWhiteType: undefined,
      },
      operatorEnabled: false,
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
    this.operatorEnabled = isAdmin()
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
      this.$router.push(`/licenses/licensesDetail/${row.licenseId}`)
    },
    handheTableSearch() {
      this.queryParams.page = 1
      this.getData()
    },
      addBlackWhite(item, blackWhiteType) {
      // blackWhite({licenseId: item.licenseId, blackWhiteType: blackWhiteType}).then((res) => {
      //   this.$notification["success"]({
      //     message : this.$t('Setting.OperationSuccessful')
      //   })
      //   this.handheTableSearch()
      // }).finally(() => {
      // })
          if(blackWhiteType){
              let data = {
                  id: undefined,
                  type: blackWhiteType,
                  category: 'LICENSE',
                  domain: 'PLATFORM',
                  tag: 'LATEST',
                  identifier: item.licenseId,
                  validFrom: undefined,
              }
              addAllowlistDenylistBlock(data).then(res=>{
                  this.$notification["success"]({
                      message : this.$t('Setting.OperationSuccessful')
                  })
              }).catch(err=>{
                  this.$notification['error']({
                      message: err.response.data.error,
                      description: ''
                  })
              }).finally(()=>{
                  this.handheTableSearch()
              })
          }
    },

    removeBlackWhite(item, blackWhiteType){
        if(blackWhiteType){
            let data = {
                id: undefined,
                type: blackWhiteType,
                category: 'LICENSE',
                domain: 'PLATFORM',
                tag: 'LATEST',
                identifier: item.licenseId,
                correlationId:undefined,
                validFrom: undefined,
            }
            deleteAllowlistDenylistBlock(data).then(res => {
                this.$notification["success"]({
                    message : this.$t('Setting.OperationSuccessful')
                })
            }).catch((err) => {
                this.$notification["error"]({
                    message: err.response.data.error,
                })
            }).finally(() => {
                this.handheTableSearch()
            })
        }

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
.black-white-type-select {
  width: 200px;
}

.o-btn {
  width: 36px;
  height: 36px;
  margin-right: 8px;
  background-color: #1890FF;
  border-radius: 8px;
  display: inline-flex;
  justify-content: center;
  align-items: center;
  margin-top: 8px;
}

.o-btn img {
  width: 20px;
  height: 20px;
  cursor: pointer;
}

.o-graph {
  background-color: #7adcfc;
}

.o-black {
  background-color: #f58080
}

.o-rm {
  background-color: #d81e06
}
</style>
