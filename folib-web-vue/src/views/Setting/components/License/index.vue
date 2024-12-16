<template>
  <div class="wrapper">
    <a-card class="header-solid" :bordered="false" :title="blackWhiteType == 1?$t('Setting.LicenseWhiteList'):$t('Setting.LicenseBlacklist')">
      <div slot="extra">
        <a-tooltip @click="addLicenseShow">
          <template slot="title">{{ $t('Setting.Add') }}</template>
          <a-icon type="plus-circle" theme="filled" class="cursor-pointer package-name-add mr-20"
            :style="{ fontSize: '28px', color: '#1890FF' }" />
        </a-tooltip>
        <a-input-search :placeholder="$t('Licenses.EnterLicenseQuery')" class="v-search"
          v-model="queryParams.searchKeyword" @search="handheTableSearch()" />
      </div>
      <a-table rowKey="uuid" class="mt-20" :columns="i18nColumns2" :data-source="licenseData"
        @change="handleChangeTable" :scroll="{ x: true }" :loading="licenseTableLoading"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }">
        <template slot="identifier" slot-scope="identifier, row">
          <a-button type="link" @click="handleGoDetail(row.identifier)">
            {{ identifier }}
          </a-button>
        </template>
        <div slot="validFrom" slot-scope="text, record">
          <span>{{ formatDate(record.validFrom)}}</span>
        </div>
        <div slot="operation" slot-scope="text, record">
          <div class="col-action">
            <a-popconfirm :title="$t('Package.SureDelete')" okType="danger" :ok-text="$t('Package.Confirm')"
              :cancel-text="$t('Package.Cancel')" @confirm="removeAllowlistDenylistBlock(record)">
              <a-button type="link" size="small">
                <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                    d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                    fill="#111827" />
                </svg>
                <span class="text-danger">DELETE</span>
              </a-button>
            </a-popconfirm>
            <a-button type="link" size="small" @click="enditLicense(record)">
              <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                   xmlns="http://www.w3.org/2000/svg">
                <path class="fill-muted"
                      d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                      fill="#111827"/>
                <path class="fill-muted"
                      d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                      fill="#111827"/>
              </svg>
              <span class="text-dark" >EDIT</span>
            </a-button>
          </div>
        </div>
      </a-table>
      <AddLicense v-if="showLicenseModal" :modelVisible="showLicenseModal" :blackWhiteType="blackWhiteType" :recordData="recordData"
        @licenseModalCancel="licenseModalCancel" @licenseRefresh="handheTableSearch" />
    </a-card>
  </div>
</template>

<script>
import { hasRole, isAdmin, hasPermission } from "@/utils/permission";
import { getLicensesList, blackWhite } from "@/api/licenses.js";
import { formatTimestamp } from "@/utils/util.js";
import AddLicense from "./add"
import {deleteAllowlistDenylistBlock, queryAllowlistDenylistBlock} from "@/api/AllowlistDenylistBlock";
import moment from "moment/moment";
export default {
  props: {
    blackWhiteType: {
      type: Number,
      default: 0,
    },
  },
  components: {
    AddLicense,
  },
  data() {
    return {
      columns2: [
        // {
        //   title: "许可证编号",
        //   i18nKey: 'Licenses.LicenseNumber',
        //   dataIndex: "licenseId",
        //   scopedSlots: { customRender: "licenseId" },
        // },
        {
          title: '许可证编号',
          i18nKey: 'Licenses.LicenseNumber',
          dataIndex: 'identifier',
          key: 'identifier',
          width: '40%',
          scopedSlots: {customRender: 'identifier'},
        },
        {
          title: '有效期',
          i18nKey: 'BlackWhite.validFrom',
          dataIndex: 'validFrom',
          width: '40%',
          scopedSlots: {customRender: 'validFrom'},
          key: 'validFrom'
        },
        {
          title: '操作',
          i18nKey: 'BlackWhite.Operate',
          dataIndex: 'operation',
          width: '20%',
          align: 'right',
          scopedSlots: {customRender: 'operation'},
        }
      ],
      licenseData: [],
      licenseTableLoading: false,
      queryParams: {
        page: 1,
        limit: 5,
        total: 0,
        searchKeyword: '',
        blackWhiteType: undefined,
      },
      operatorEnabled: false,
      showLicenseModal: false,
      recordData: undefined,
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
    if (this.blackWhiteType) {
      this.queryParams.blackWhiteType = this.blackWhiteType
    }
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      //this.licenseTableLoading = true
      // getLicensesList(this.queryParams).then((res) => {
      //   this.queryParams.total = res.data.total
      //   this.licenseData = res.data.rows
      // }).finally(() => {
      //   this.licenseTableLoading = false
      // })
      if(this.blackWhiteType === 1){
        this.getAllowlistDenylistData( {type: 'WHITES',category:'LICENSE',domain: 'PLATFORM',page:this.queryParams.page,size:this.queryParams.limit,identifier:this.queryParams.searchKeyword});
      }else {
        this.getAllowlistDenylistData({type: 'BLACKLIST',category:'LICENSE',domain: 'PLATFORM',page:this.queryParams.page,size:this.queryParams.limit,identifier:this.queryParams.searchKeyword});
      }


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
    handleGoDetail(licenseId) {
      this.$router.push(`/licenses/licensesDetail/${licenseId}`)
    },
    handheTableSearch() {
      this.queryParams.page = 1
      this.getData()
    },
    blackWhite(item, blackWhiteType) {
      blackWhite({ licenseId: item.licenseId, blackWhiteType: blackWhiteType }).then((res) => {
        this.$notification["success"]({
          message: this.$t('Package.OperateSuccess'),
        })
        this.handheTableSearch()
      }).finally(() => {
      })
    },

    removeAllowlistDenylistBlock(data) {
      deleteAllowlistDenylistBlock(data).then(res => {
        this.$notification["success"]({
          message: this.$t('Package.OperateSuccess'),
        })
      }).finally(() => {
        this.handheTableSearch()
      })
    },
    enditLicense(data){
      this.recordData = data;
      this.showLicenseModal = true
    },
    licenseModalCancel() {
      this.showLicenseModal = false
    },
    addLicenseShow() {
      this.showLicenseModal = true
    },

    getAllowlistDenylistData(data){
      this.licenseTableLoading = true;
      queryAllowlistDenylistBlock(data).then(res=>{
        this.licenseData = res.data.content;
        this.queryParams.total = res.data.totalElements;
      }).finally(()=>{
        this.licenseTableLoading = false
      })
    },

    formatDate(date) {
      console.log('date',date)
      if(date){
        return moment(date).format('YYYY-MM-DD');
      }
     return "";
    }
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
