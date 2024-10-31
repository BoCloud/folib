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
            <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.whiteList"
              :pagination="vulnerabilities.whiteList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.whiteList.length, showLessItems: true }">
              <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
                <label>{{ item }}</label>
                <template #extra>
                  <a-popconfirm :title="$t('Setting.SureRemovedWhitelist')" :ok-text="$t('Setting.BeSure')"
                    :cancel-text="$t('Setting.Cancel')" class="d-popconfirm" @confirm="removeWhite(item)">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-danger">DELETE</span>
                  </a-popconfirm>
                </template>
              </a-list-item>
            </a-list>
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
            <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.blackList"
              :pagination="vulnerabilities.blackList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.blackList.length, showLessItems: true }">
              <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
                {{ item }}
                <template #extra>
                  <a-popconfirm :title="$t('Setting.SureRemovedBlacklisted')" :ok-text="$t('Setting.BeSure')"
                    :cancel-text="$t('Setting.Cancel')" class="d-popconfirm" @confirm="removeBlack(item)">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-danger">DELETE</span>
                  </a-popconfirm>
                </template>
              </a-list-item>
            </a-list>
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
      @ok="addVulnerabilities()" centered>
      <a-form-model layout="horizontal" ref="vulnerabilitiesForm" :model="vulnerabilitiesForm" :rules="vulnerabilitiesRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :colon="false" prop="uuid">
              <a-input v-model="vulnerabilitiesForm.uuid" :placeholder="$t('Setting.EnterVulnerabilityNum')" />
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
  removeVulnerabilitiesWhite,
  removeVulnerabilitiesBlack,
  saveOrUpdateVulnerabilityNotify,
  securityPolicyConfig,
  securityPolicyBlock,
  securityPolicyAddPackageName,
  securityPolicyDeletePackageName
} from '@/api/folib'

import License from '../License/index.vue'
import PackageName from "../Package/index.vue"

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
        uuid: [
          { required: true, trigger: ['blur'], validator: checkVulnerabilityNum},
        ],
      },
      vulnerabilitiesForm: {
        uuid: undefined,
      },
      showVulnerabilitiesModal: false,
      vulnerabilitiesType: null,
    }
  },
  components: {
    License,
    PackageName,
  },
  computed: {

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
      this.getVulnerabilities(-1)
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
    removeWhite(uuid) {
      removeVulnerabilitiesWhite({ white: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.RemovedWhitelistSuccess'))
        })
        .finally(() => {
          this.getVulnerabilities(1)
        })
    },
    removeBlack(uuid) {
      removeVulnerabilitiesBlack({ black: uuid })
        .then(res => {
          this.successMsg(uuid + this.$t('Setting.RemovedBlacklistSuccess'))
        })
        .finally(() => {
          this.getVulnerabilities(2)
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
        uuid: undefined,
      }
      if (this.$refs.vulnerabilitiesForm) {
        this.$refs.vulnerabilitiesForm.resetFields()
      }
    },
    addVulnerabilities() {
      this.$refs.vulnerabilitiesForm.validate((valid) => {
        if (valid) {
          if (!this.vulnerabilitiesForm.uuid) {
            this.$notification['warning']({
              message: this.$t('Setting.EnterVulnerabilityNum'),
              description: ''
            })
            return
          }
          if (this.vulnerabilitiesType === 1) {
            this.addWhite(this.vulnerabilitiesForm.uuid)
          } else if (this.vulnerabilitiesType === 2) {
            this.addBlack(this.vulnerabilitiesForm.uuid)
          }
        }
      })
    },
    handheWhiteListSearch() {
      if (this.searchWhiteList) {
        this.vulnerabilities.whiteList = this.vulnerabilities.sourceWhiteList.filter(item => item.includes(this.searchWhiteList))
      } else {
        this.vulnerabilities.whiteList = this.vulnerabilities.sourceWhiteList
      }
    },
    handheBlackListSearch() {
      if (this.searchBlackList) {
        this.vulnerabilities.blackList = this.vulnerabilities.sourceBlackList.filter(item => item.includes(this.searchBlackList))
      } else {
        this.vulnerabilities.blackList = this.vulnerabilities.sourceBlackList
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
</style>
