<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-card class="header-solid white-card" id="white" :title="$t('Setting.VulnerabilityWhiteList')">
          <div slot="extra">
            <a-tooltip @click="addVulnerabilitiesModal(1)">
              <template slot="title">{{ $t('Setting.Add') }}</template>
              <a-icon type="plus-circle" theme="filled" class="cursor-pointer package-name-add mr-20"
                :style="{ fontSize: '28px', color: '#1890FF' }" />
            </a-tooltip>
            <a-input-search :placeholder="$t('Setting.EnterVulnerabilityQuery')" class="v-search"  v-model="searchWhiteList" @search="handheWhiteListSearch()"/>
          </div>
          <div class="white-group">
<!--            <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.whiteList"-->
<!--              :pagination="vulnerabilities.whiteList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.whiteList.length, showLessItems: true }">-->
<!--              <a-list-item slot="renderItem" :key="index" slot-scope="item, index">-->
<!--                <label>{{ item }}</label>-->
<!--                <template #extra>-->
<!--                  <a-popconfirm :title="$t('Setting.SureRemovedWhitelist')" :ok-text="$t('Setting.BeSure')"-->
<!--                    :cancel-text="$t('Setting.Cancel')" class="d-popconfirm" @confirm="removeWhite(item)">-->
<!--                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">-->
<!--                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"-->
<!--                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"-->
<!--                        fill="#111827" />-->
<!--                    </svg>-->
<!--                    <span class="text-danger">DELETE</span>-->
<!--                  </a-popconfirm>-->
<!--                </template>-->
<!--              </a-list-item>-->
<!--            </a-list>-->

            <a-table :columns="i18nColumns" :data-source="vulnWhiteList" :scroll="{ x: true }"
                     :pagination="{ pageSize: vulnWhiteData.pageSize, current: vulnWhiteData.pageNumber, total: vulnWhiteData.total, showLessItems: true }"
                     @change="handleChangevulnWhiteListTable" :loading="vulnWhiteListLoading"
                     :row-key="(r, i) => i.toString()" style="width: 100%">
              <div slot="validFrom" slot-scope="text, record">
                <span>{{ formatDate(record.validFrom)}}</span>
              </div>
              <div slot="operation" slot-scope="text, record">
                <div class="col-action">
                  <a-popconfirm :title="$t('Setting.SureRemovedWhitelist')" okType="danger"
                                :ok-text="$t('Setting.BeSure')"
                                @confirm="removeWhite(record)"
                                :cancel-text="$t('Setting.Cancel')">
                    <a-button type="link" size="small" >
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                           xmlns="http://www.w3.org/2000/svg">
                        <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                              d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                              fill="#111827"/>
                      </svg>
                      <span class="text-danger">DELETE</span>
                    </a-button>
                  </a-popconfirm>
                  <a-button type="link" size="small" @click="showVulnWhiteDatils(record)">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                         xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-muted"
                            d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                            fill="#111827"/>
                      <path class="fill-muted"
                            d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                            fill="#111827"/>
                    </svg>
                    <span class="text-dark">EDIT</span>
                  </a-button>
                </div>
              </div>
            </a-table>
          </div>
        </a-card>
      </a-col>
      <a-col :span="24" class="mb-24">
        <a-card class="header-solid black-card" id="black" :title="$t('Setting.VulnerabilityBlacklist')">
          <div slot="extra">
            <a-tooltip @click="addVulnerabilitiesModal(2)">
              <template slot="title">{{ $t('Setting.Add') }}</template>
              <a-icon type="plus-circle" theme="filled" class="cursor-pointer package-name-add mr-20"
                :style="{ fontSize: '28px', color: '#1890FF' }" />
            </a-tooltip>
            <a-input-search :placeholder="$t('Setting.EnterVulnerabilityQuery')" class="v-search"  v-model="searchBlackList" @search="handheBlackListSearch()"/>
          </div>
          <div class="black-group">
<!--            <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.blackList"-->
<!--              :pagination="vulnerabilities.blackList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.blackList.length, showLessItems: true }">-->
<!--              <a-list-item slot="renderItem" :key="index" slot-scope="item, index">-->
<!--                {{ item }}-->
<!--                <template #extra>-->
<!--                  <a-popconfirm :title="$t('Setting.SureRemovedBlacklisted')" :ok-text="$t('Setting.BeSure')"-->
<!--                    :cancel-text="$t('Setting.Cancel')" class="d-popconfirm" @confirm="removeBlack(item)">-->
<!--                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">-->
<!--                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"-->
<!--                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"-->
<!--                        fill="#111827" />-->
<!--                    </svg>-->
<!--                    <span class="text-danger">DELETE</span>-->
<!--                  </a-popconfirm>-->
<!--                </template>-->
<!--              </a-list-item>-->
<!--            </a-list>-->

            <a-table :columns="vulnColumns" :data-source="vulnDenylist" :scroll="{ x: true }"
                     :pagination="{ pageSize: vulnDenylistData.pageSize, current: vulnDenylistData.pageNumber, total: vulnDenylistData.total, showLessItems: true }"
                     @change="handleChangevulnDenylistTable" :loading="vulnDenylistLoading"
                     :row-key="(r, i) => i.toString()" style="width: 100%">
              <div slot="validFrom" slot-scope="text, record">
                <span>{{ formatDate(record.validFrom)}}</span>
              </div>
              <div slot="operation" slot-scope="text, record">
                <div class="col-action">
                  <a-popconfirm :title="$t('Package.SureDelete')" okType="danger"
                                :ok-text="$t('Package.Confirm')"
                                @confirm="removeBlack(record)"
                                :cancel-text="$t('Package.Cancel')">
                    <a-button type="link" size="small" >
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                           xmlns="http://www.w3.org/2000/svg">
                        <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                              d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                              fill="#111827"/>
                      </svg>
                      <span class="text-danger">DELETE</span>
                    </a-button>
                  </a-popconfirm>
                  <a-button type="link" size="small" @click="showVulnDenylistDatils(record)">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                         xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-muted"
                            d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                            fill="#111827"/>
                      <path class="fill-muted"
                            d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                            fill="#111827"/>
                    </svg>
                    <span class="text-dark">EDIT</span>
                  </a-button>
                </div>
              </div>
            </a-table>
          </div>
        </a-card>
      </a-col>
    </a-row>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <License :blackWhiteType="1"/>
      </a-col>
      <a-col :span="24" class="mb-24">
        <License :blackWhiteType="2"/>
      </a-col>
    </a-row>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <PackageName />
      </a-col>
    </a-row>
    <a-modal v-model="showVulnerabilitiesModal"
      :title="vulnerabilitiesType === 1 ? $t('Setting.AddWhitelist') : $t('Setting.AddBlacklist')" :maskClosable="false"
      :cancelText="$t('Setting.Cancel')" :okText="$t('Setting.BeSure')" @cancel="vulnerabilitiesModalCancel()"
      @ok="handleVulnerabilities()" centered>
      <a-form-model layout="horizontal" ref="vulnerabilitiesForm" :model="vulnerabilitiesForm" :rules="vulnerabilitiesRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :colon="false" prop="identifier" :label="$t('Setting.vulnerabilityId')" >
              <a-input v-model="vulnerabilitiesForm.identifier" :disabled="identifierDisabled" :placeholder="$t('Setting.EnterVulnerabilityNum')" />
            </a-form-model-item>

            <a-form-model-item class="mb-10" :colon="false" :label="$t('Setting.validFrom')" >
              <a-date-picker  :default-value="vulnWhiteListMonthLater" v-model="vulnerabilitiesForm.validFrom" style="width: 100%" />
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>
<script>

import {
  addVulnerabilitiesWhite,
  addVulnerabilitiesBlack,
  securityPolicyConfig,
} from '@/api/folib'

import License from '../License/index.vue'
import PackageName from "../Package/index.vue"
import {
  addAllowlistDenylistBlock,
  deleteAllowlistDenylistBlock,
  queryAllowlistDenylistBlock, updateAllowlistDenylistBlock
} from "@/api/AllowlistDenylistBlock";
import moment from 'moment';
export default {
  props: {
    securityPolicyActiveKey: {
      type: String,
      default: "",
    },
  },
  data() {
    const checkVulnerabilityNum = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.EnterVulnerabilityNum')))
      } else {
				callback()
			}
    }
    return {
      vulnerabilities: {
        whiteList: [],
        blackList: [],
        sourceWhiteList: [],
        sourceBlackList: [],
      },
      searchWhiteList: undefined,
      searchBlackList: undefined,
      vulnerabilitiesRules: {
        identifier: [
          { required: true, trigger: ['blur'], validator: checkVulnerabilityNum},
        ],
      },
      vulnerabilitiesForm: {
        identifier: undefined,
        validFrom: undefined,
        id: undefined,
      },
      showVulnerabilitiesModal: false,
      vulnerabilitiesType: null,
      vulnWhiteList:[],
      vulnDenylist:[],
      vulnColumns:[
        {
          title: '漏洞编号',
          i18nKey: 'BlackWhite.vulnerabilityId',
          dataIndex: 'identifier',
          key: 'identifier',
          width: '40%',
          scopedSlots: {customRender: 'identifier'},
        },
        {
          title: '有效期',
          i18nKey: 'BlackWhite.validFrom',
          dataIndex: 'validFrom',
          scopedSlots: {customRender: 'validFrom'},
          width: '40%',
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
      vulnWhiteData:{
        pageSize:5,
        pageNumber:1,
        total:0,
      },
      vulnDenylistData:{
        pageSize:5,
        pageNumber:1,
        total:0,
      },
      vulnWhiteListLoading: false,
      vulnDenylistLoading: false,
      identifierDisabled: false,
      vulnWhiteListMonthLater: undefined,//this.calculateOneMonthLater(),
       isEdit: false,

    }
  },
  components: {
    License,
    PackageName,
  },
  computed: {
    i18nColumns() {
      return this.vulnColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  created() {
    this.initData()
  },
  watch: {
    'securityPolicyActiveKey': function (newval) {
      if (newval === "1") {
        this.initData()
      }
    },
  },
  mounted() { },
  methods: {
    successMsg(message) {
      if (!message) {
        message = this.$t('Setting.OperationSuccessful');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    initData() {
      //this.getVulnerabilities(-1)
      this. getVulnAllowlistData( {type: 'WHITES',category:'VULNERABILITY',page:this.vulnWhiteData.pageNumber,size:this.vulnWhiteData.pageSize});
      this. getVulnDenylistData({type: 'BLACKLIST',category:'VULNERABILITY',page:this.vulnDenylistData.pageNumber,size:this.vulnDenylistData.pageSize});
    },
    getVulnerabilities(type) {
      securityPolicyConfig()
        .then(res => {
          if (type !== 2) {
            this.vulnerabilities.whiteList = res.whites
            this.vulnerabilities.sourceWhiteList = res.whites
          }
          if (type !== 1) {
            this.vulnerabilities.blackList = res.blacks
            this.vulnerabilities.sourceBlackList = res.blacks
          }
          if (type == -1 ) {
            this.vulnerabilities.whiteList = res.whites
            this.vulnerabilities.blackList = res.blacks
            this.vulnerabilities.sourceWhiteList = res.whites
            this.vulnerabilities.sourceBlackList = res.blacks
          }
        })
        .finally(() => { })


    },

    getVulnAllowlistData(data){
      this.vulnWhiteListLoading = true;
      queryAllowlistDenylistBlock(data).then(res=>{
        this.vulnWhiteList = res.data.rows;
        this.vulnWhiteData.total = res.data.total;
      }).finally(()=>{
        this.vulnWhiteListLoading = false
      })
    },

    getVulnDenylistData(data){
      queryAllowlistDenylistBlock(data).then(res=>{
        this.vulnDenylist = res.data.rows;
        this.vulnDenylistData.total = res.data.total;
      }).finally(()=>{
        this.vulnDenylistLoading = false
      })
    },


    removeWhite(data) {
      deleteAllowlistDenylistBlock(data).then(res => {
        this.successMsg(data.identifier + this.$t('Setting.RemovedWhitelistSuccess'))
      }).finally(() => {
        this. getVulnAllowlistData( {type: 'WHITES',category:'VULNERABILITY',pageNumber:this.vulnWhiteData.pageNumber,pageSize:this.vulnWhiteData.pageSize});
      })
    },
    removeBlack(data) {
      deleteAllowlistDenylistBlock(data).then(res => {
        this.successMsg(data.identifier + this.$t('Setting.RemovedBlacklistSuccess'))
      }).finally(() => {
        this.getVulnDenylistData( {type: 'BLACKLIST',category:'VULNERABILITY',pageNumber:this.vulnWhiteData.pageNumber,pageSize:this.vulnWhiteData.pageSize});
      })
    },
    addWhite(uuid) {
      addVulnerabilitiesWhite({ white: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.AddWhitelistSuccess'))
        })
        .catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        })
        .finally(() => {
          this.resetVulnerabilitiesForm()
          this.showVulnerabilitiesModal = false
          this.getVulnerabilities(1)
        })
    },
    addBlack(uuid) {
      addVulnerabilitiesBlack({ black: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.AddBlacklistSuccess'))
        })
        .catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        })
        .finally(() => {
          this.resetVulnerabilitiesForm()
          this.showVulnerabilitiesModal = false
          this.getVulnerabilities(2)
        })
    },
    vulnerabilitiesModalCancel() {
      this.showVulnerabilitiesModal = false
    },
    addVulnerabilitiesModal(type) {
      this.resetVulnerabilitiesForm()
      this.showVulnerabilitiesModal = true
      this.vulnerabilitiesType = type
    },
    resetVulnerabilitiesForm() {
      this.vulnerabilitiesForm =  {
        identifier: undefined,
        validFrom: undefined,
      }
      this.isEdit = false;
      this.identifierDisabled =false;
      if (this.$refs.vulnerabilitiesForm) {
        this.$refs.vulnerabilitiesForm.resetFields()
      }
    },
    handleVulnerabilities(){
      this.$refs.vulnerabilitiesForm.validate((valid) => {
            if (valid) {
              if (!this.vulnerabilitiesForm.identifier) {
                this.$notification['warning']({
                  message: this.$t('Setting.EnterVulnerabilityNum'),
                  description: ''
                })
                return
              }
              let data = {
                id:this.vulnerabilitiesForm.id,
                type: undefined,
                domain: 'PLATFORM',
                tag: 'LATEST',
                category: 'VULNERABILITY',
                identifier: this.vulnerabilitiesForm.identifier,
                validFrom: this.vulnerabilitiesForm.validFrom ? moment(this.vulnerabilitiesForm.validFrom).format('YYYY-MM-DD HH:mm:ss') : undefined,
              }
              if (this.isEdit){
                this.updateVulnerabilities(data)
              }else {
                this.addVulnerabilities(data)
              }
            }
          });
    },
    updateVulnerabilities(data) {
      if (this.vulnerabilitiesType === 1) {
        //this.addWhite(this.vulnerabilitiesForm.uuid)
        data.type='WHITES';
        updateAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(data.identifier + this.$t('Setting.UpdateWhitelistSuccess'))
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally(()=>{
          this.resetVulnerabilitiesForm()
          this.showVulnerabilitiesModal = false
          this. getVulnAllowlistData( {type: 'WHITES',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnWhiteData.pageNumber,size:this.vulnWhiteData.pageSize});
        })
      } else if (this.vulnerabilitiesType === 2) {
        //this.addBlack(this.vulnerabilitiesForm.uuid)
        data.type='BLACKLIST';
        updateAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(data.identifier + this.$t('Setting.UpdateBlacklistSuccess'))
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally(()=>{
          this.resetVulnerabilitiesForm()
          this.showVulnerabilitiesModal = false
          this. getVulnDenylistData( {type: 'BLACKLIST',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnWhiteData.pageNumber,size:this.vulnWhiteData.pageSize});
        })
      }
    },
    addVulnerabilities(data) {
      if (this.vulnerabilitiesType === 1) {
        //this.addWhite(this.vulnerabilitiesForm.uuid)
        data.type='WHITES';
        addAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(data.identifier + this.$t('Setting.AddWhitelistSuccess'))
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally(()=>{
          this.resetVulnerabilitiesForm()
          this.showVulnerabilitiesModal = false
          this. getVulnAllowlistData( {type: 'WHITES',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnWhiteData.pageNumber,size:this.vulnWhiteData.pageSize});
        })
      } else if (this.vulnerabilitiesType === 2) {
        //this.addBlack(this.vulnerabilitiesForm.uuid)
        data.type='BLACKLIST';
        addAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(data.identifier + this.$t('Setting.AddBlacklistSuccess'))
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally(()=>{
          this.resetVulnerabilitiesForm()
          this.showVulnerabilitiesModal = false
          this. getVulnDenylistData( {type: 'BLACKLIST',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnWhiteData.pageNumber,size:this.vulnWhiteData.pageSize});
        })
      }
    },
    handheWhiteListSearch() {
      this.getVulnAllowlistData( {type: 'WHITES',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnWhiteData.pageNumber,size:this.vulnWhiteData.pageSize,identifier:this.searchWhiteList});
    },
    handheBlackListSearch() {
      this.getVulnDenylistData({type: 'BLACKLIST',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnDenylistData.pageNumber,size:this.vulnDenylistData.pageSize,identifier:this.searchBlackList});
    },
    handleChangevulnWhiteListTable(pagination, filters, sorter) {
        if (pagination) {
          this.vulnWhiteData.pageNumber = pagination.current
        }
      this. getVulnAllowlistData( {type: 'WHITES',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnWhiteData.pageNumber,size:this.vulnWhiteData.pageSize});
    },
    handleChangevulnDenylistTable(pagination, filters, sorter){
      if (pagination) {
        this.vulnDenylistData.pageNumber = pagination.current
      }
      this. getVulnDenylistData( {type: 'BLACKLIST',category:'VULNERABILITY',domain: 'PLATFORM',page:this.vulnDenylistData.pageNumber,size:this.vulnDenylistData.pageSize});
    },
    showVulnWhiteDatils(data) {
      this.resetVulnerabilitiesForm()
      this.showVulnerabilitiesModal = true
      this.vulnerabilitiesType = 1;
      this.vulnerabilitiesForm.identifier = data.identifier
      this.vulnerabilitiesForm.validFrom = data.validFrom
      this.identifierDisabled = true
      this.isEdit = true
      this.vulnerabilitiesForm.id = data.id
    },
    showVulnDenylistDatils(data) {
      this.resetVulnerabilitiesForm()
      this.showVulnerabilitiesModal = true
      this.vulnerabilitiesType = 2;
      this.vulnerabilitiesForm.identifier = data.identifier
      this.vulnerabilitiesForm.validFrom = data.validFrom
      this.identifierDisabled = true
      this.isEdit = true
      this.vulnerabilitiesForm.id = data.id
    },
    calculateOneMonthLater() {
      const currentDate = new Date();
      const oneMonthLater = new Date(currentDate);
      oneMonthLater.setMonth(currentDate.getMonth() + 6);
      return moment(oneMonthLater).startOf('day');
    },

    formatDate(date) {
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
</style>
