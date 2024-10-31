<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
          :title="$t('BlockStrategy.StrategyList')">
          <div slot="extra">
            <a-tooltip @click="showBlockStrategyInfo(null)">
              <template slot="title">{{ $t('Setting.Add') }}</template>
              <a-icon type="plus-circle" theme="filled" class="cursor-pointer package-name-add mr-10"
                :style="{ fontSize: '28px', color: '#1890FF' }" />
            </a-tooltip>
            <a-input-search :placeholder="$t('BlockStrategy.EnterBlockStrategyNameQuery')" class="v-search"
              v-model="blockStrategyQuery.blockStrategyName" @search="handheSearch()" />
            <a-input-search :placeholder="$t('BlockStrategy.EnterStorageIdQuery')" class="v-search"
              v-model="blockStrategyQuery.storageId" @search="handheSearch()" />
            <a-input-search :placeholder="$t('BlockStrategy.EnterRepositoryIdQuery')" class="v-search"
              v-model="blockStrategyQuery.repositoryId" @search="handheSearch()" />
            <a-input-search :placeholder="$t('BlockStrategy.EnterPackageNameQuery')" class="v-search"
              v-model="blockStrategyQuery.packageName" @search="handheSearch()" />
            <a-input-search :placeholder="$t('BlockStrategy.EnterLicenseIdQuery')" class="v-search"
              v-model="blockStrategyQuery.licenseId" @search="handheSearch()" />
          </div>
          <a-table :columns="i18nBlockStrategyColumns" :data-source="blockStrategyList" :scroll="{ x: true }"
            @change="handleChangeTable" :loading="blockStrategyLoading"
            :pagination="{ pageSize: blockStrategyQuery.limit, current: blockStrategyQuery.page, total: blockStrategyQuery.total, showLessItems: true }"
            :row-key="(r, i) => i.toString()">
            <template slot="vulnerabilityLevels" slot-scope="vulnerabilityLevels">
              <span v-if="!vulnerabilityLevels">
              </span>
              <span v-else>
                <span v-if="vulnerabilityLevels.includes('CRITICAL')">
                  <a-tag color="#f86c6b">
                    {{ $t('BlockStrategy.Seriously') }}
                  </a-tag>
                </span>
                <span v-if="vulnerabilityLevels.includes('HIGH')">
                  <a-tag color="#fd8c00">
                    {{ $t('BlockStrategy.HighRisk') }}
                  </a-tag>
                </span>
                <span v-if="vulnerabilityLevels.includes('MEDIUM')"> 
                  <a-tag color="#ffc107">{{ $t('BlockStrategy.MediumRisk') }}
                  </a-tag>
                </span>
                <span v-if="vulnerabilityLevels.includes('LOW')">
                  <a-tag color="#4dbd74">{{ $t('BlockStrategy.LowRisk') }}
                  </a-tag>
                </span>
              </span>
            </template>
            <template slot="filterVulnerabilityWhites" slot-scope="filterVulnerabilityWhites">
              <span v-if="filterVulnerabilityWhites"><a-tag color="green">{{ $t('BlockStrategy.YES') }}</a-tag></span>
              <span v-else><a-tag color="orange">{{ $t('BlockStrategy.NO') }}</a-tag></span>
            </template>
            <template slot="filterVulnerabilityBlacks" slot-scope="filterVulnerabilityBlacks">
              <span v-if="filterVulnerabilityBlacks"><a-tag color="green">{{ $t('BlockStrategy.YES') }}</a-tag></span>
              <span v-else><a-tag color="orange">{{ $t('BlockStrategy.NO') }}</a-tag></span>
            </template>
            <template slot="filterLicenseWhites" slot-scope="filterLicenseWhites">
              <span v-if="filterLicenseWhites"><a-tag color="green">{{ $t('BlockStrategy.YES') }}</a-tag></span>
              <span v-else><a-tag color="orange">{{ $t('BlockStrategy.NO') }}</a-tag></span>
            </template>
            <template slot="filterLicenseBlacks" slot-scope="filterLicenseBlacks">
              <span v-if="filterLicenseBlacks"><a-tag color="green">{{ $t('BlockStrategy.YES') }}</a-tag></span>
              <span v-else><a-tag color="orange">{{ $t('BlockStrategy.NO') }}</a-tag></span>
            </template>
            <template slot="filterAllPackageName" slot-scope="filterAllPackageName">
              <span v-if="filterAllPackageName"><a-tag color="green">{{ $t('BlockStrategy.YES') }}</a-tag></span>
              <span v-else><a-tag color="orange">{{ $t('BlockStrategy.NO') }}</a-tag></span>
            </template>
            <!-- <template slot="filterAllLicense" slot-scope="filterAllLicense">
              <span v-if="filterAllLicense">是</span>
              <span v-else>否</span>
            </template> -->
            <div slot="operation" slot-scope="text, record">
              <div class="col-action">
                <a-popconfirm :title="$t('Package.SureDelete')" okType="danger" :ok-text="$t('Package.Confirm')"
                  :cancel-text="$t('Package.Cancel')" @confirm="blockStrategyDelete(record)">
                  <a-button type="link" size="small">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-danger">DELETE</span>
                  </a-button>
                </a-popconfirm>
                <a-button type="link" size="small" @click="showBlockStrategyInfo(record)">
                  <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path class="fill-muted"
                      d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                      fill="#111827" />
                    <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                      fill="#111827" />
                  </svg>
                  <span class="text-dark">EDIT</span>
                </a-button>
              </div>
            </div>
          </a-table>
        </a-card>
      </a-col>
    </a-row>
    <a-drawer placement="right" width="60%" :title="$t('BlockStrategy.BlockingStrategySetting')"
      v-if="blockStrategyVisible" :visible="blockStrategyVisible" @close="blockStrategyClose" :zIndex="100">
      <a-card :bordered="false" class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
        <a-form-model layout="horizontal" ref="blockStrategyFormRef" :model="blockStrategyForm"
          :rules="blockStrategyRules" :hideRequiredMark="false">
          <a-form-model-item class="mb-10" :label="$t('BlockStrategy.BlockingStrategyName')" :colon="false"
            prop="blockStrategyName">
            <a-input :placeholder="$t('BlockStrategy.EnterBlockingStrategyName')"
              v-model="blockStrategyForm.blockStrategyName" :style="{ width: '950px', }" />
          </a-form-model-item>
          <a-form-model-item class="mb-10" :label="$t('BlockStrategy.ChooseBlockRepository')" :colon="false"
            prop="accessToken">
            <a-transfer :data-source="repositoriesList"
              :titles="[$t('BlockStrategy.SelectableRepositories'), $t('BlockStrategy.SelectedRepositories')]"
              :render="item => item.title" :target-keys="selectedRepositoriesKeys" :disabled="false" show-search
              :filter-option="(inputValue, item) => item.key.indexOf(inputValue) !== -1" show-select-all
              @change="repositoriesOnChange" @scroll="handleRepositoriesScroll"
              :list-style="{ width: '450px', height: '350px', }">
            </a-transfer>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="vulnerabilityLevels">
            <template slot="label">
              {{ $t('BlockStrategy.VulnerabilityBlockingLevel') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('BlockStrategy.VulnerabilityBlockingLevelTip') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-checkbox-group v-model="blockStrategyForm.vulnerabilityLevels">
              <a-checkbox value="CRITICAL">
                {{ $t('BlockStrategy.Seriously') }}
              </a-checkbox>
              <a-checkbox value="HIGH">
                {{ $t('BlockStrategy.HighRisk') }}
              </a-checkbox>
              <a-checkbox value="MEDIUM">
                {{ $t('BlockStrategy.MediumRisk') }}
              </a-checkbox>
              <a-checkbox value="LOW">
                {{ $t('BlockStrategy.LowRisk') }}
              </a-checkbox>
            </a-checkbox-group>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="filterVulnerability">
            <template slot="label">
              {{ $t('BlockStrategy.FilterVulnerabilityList') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('BlockStrategy.FilterVulnerabilityListTip1') }}</p>
                  <p class="mb-0">{{ $t('BlockStrategy.FilterVulnerabilityListTip2') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-checkbox-group v-model="blockStrategyForm.filterVulnerability">
              <a-checkbox value="white">
                {{ $t('BlockStrategy.Whitelist') }}
              </a-checkbox>
              <a-checkbox value="black">
                {{ $t('BlockStrategy.Blacklist') }}
              </a-checkbox>
            </a-checkbox-group>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="filterAllPackageName">
            <template slot="label">
              {{ $t('BlockStrategy.FullPackageNameBlocking') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('BlockStrategy.FullPackageNameBlockingTip') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-switch v-model="blockStrategyForm.filterAllPackageName" />
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="accessToken">
            <template slot="label">
              {{ $t('BlockStrategy.ChooseToBlockPackageName') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('BlockStrategy.ChooseToBlockPackageNameTip') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-transfer :data-source="packageNameBlockList"
              :titles="[$t('BlockStrategy.SelectablePackageName'), $t('BlockStrategy.SelectedPackageName')]"
              :render="item => item.title" :target-keys="selectedPackageNameBlockKeys" :disabled="false" show-search
              :filter-option="(inputValue, item) => item.key.indexOf(inputValue) !== -1" show-select-all
              @change="packageNameBlockOnChange" @search="handlePackageNameBlockSearch"
              @scroll="handlePackageNameBlockScroll" :list-style="{ width: '450px', height: '350px', }">
            </a-transfer>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="filterLicense">
            <template slot="label">
              {{ $t('BlockStrategy.FilterLicenseList') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('BlockStrategy.FilterLicenseListTip1') }}</p>
                  <p class="mb-0">{{ $t('BlockStrategy.FilterLicenseListTip2') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-checkbox-group v-model="blockStrategyForm.filterLicense">
              <a-checkbox value="white">
                {{ $t('BlockStrategy.Whitelist') }}
              </a-checkbox>
              <a-checkbox value="black">
                {{ $t('BlockStrategy.Blacklist') }}
              </a-checkbox>
            </a-checkbox-group>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="accessToken">
            <template slot="label">
              {{ $t('BlockStrategy.ChooseToBlockLicense') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('BlockStrategy.ChooseToBlockLicenseTip') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-transfer :data-source="licenseList"
              :titles="[$t('BlockStrategy.SelectableLicense'), $t('BlockStrategy.SelectedLicense')]"
              :render="item => item.title" :target-keys="selectedLicenseKeys" :disabled="false" show-search
              :filter-option="(inputValue, item) => item.key.includes(inputValue)" show-select-all
              @change="licenseOnChange" @search="handleLicenseSearch" @scroll="handleLicenseScroll"
              :list-style="{ width: '450px', height: '350px', }"></a-transfer>
          </a-form-model-item>
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="blockStrategyFormSubmit">
              {{ $t('Setting.Save') }}
            </a-button>
            <a-button class="ml-10" @click="blockStrategyClose">
              {{ $t('Setting.Cancel') }}
            </a-button>
          </a-form-model-item>
        </a-form-model>
      </a-card>
    </a-drawer>
  </div>
</template>
<script>
import {
  queryBlockStrategy,
  getBlockStrategy,
  saveBlockStrategy,
  updateBlockStrategy,
  deleteBlockStrategy,
} from "@/api/blockStrategy"
import {
  queryRepositories
} from "@/api/folib"
import {
  getPackageNameBlock,
} from "@/api/packageNameBlock"
import {
  getLicensesList,
} from "@/api/licenses"

export default {
  props: {
    securityPolicyActiveKey: {
      type: String,
      default: "",
    },
  },
  data() {
    const checkBlockStrategyName = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('BlockStrategy.EnterBlockingStrategyName')))
      } else {
        callback()
      }
    }
    return {
      blockStrategyVisible: false,
      blockStrategyForm: {
        id: '',
        blockStrategyName: '',
        vulnerabilityLevels: [],
        filterVulnerability: [],
        filterLicense: [],
        filterAllPackageName: false,
        filterAllLicense: false,
        repositories: [],
        packageNames: [],
        licenses: [],
      },
      blockStrategyRules: {
        blockStrategyName: [
          { required: true, trigger: ['blur'], validator: checkBlockStrategyName },
        ],
        vulnerabilityLevels: [
          { required: false, trigger: ['blur', 'change'] },
        ],
        filterVulnerability: [
          { required: false, trigger: ['blur', 'change'] },
        ],
        filterLicense: [
          { required: false, trigger: ['blur', 'change'] },
        ],
        filterAllPackageName: [
          { required: false, trigger: ['blur', 'change'] },
        ],
        // filterAllLicense: [
        //   { required: false, trigger: ['blur', 'change']},
        // ],
      },
      blockStrategyList: [],
      blockStrategyQuery: {
        page: 1,
        limit: 10,
        total: 0,
        blockStrategyName: undefined,
        storageId: undefined,
        repositoryId: undefined,
        blockStrategyName: undefined,
        storageId: undefined,
        repositoryId: undefined,
        packageName: undefined,
        licenseId: undefined,
      },
      blockStrategyLoading: false,
      blockStrategyColumns: [
        {
          title: '阻断策略名称',
          i18nKey: 'BlockStrategy.BlockingStrategyName',
          dataIndex: 'blockStrategyName',
          key: 'blockStrategyName',
          width: 150,
          scopedSlots: { customRender: 'blockStrategyName' },
        },
        {
          title: '漏洞阻断级别',
          i18nKey: 'BlockStrategy.VulnerabilityBlockingLevel',
          dataIndex: 'vulnerabilityLevels',
          key: 'vulnerabilityLevels',
          width: 200,
          scopedSlots: { customRender: 'vulnerabilityLevels' },
        },
        {
          title: '过滤漏洞白名单',
          i18nKey: 'BlockStrategy.FilterVulnerabilityWhitelist',
          dataIndex: 'filterVulnerabilityWhites',
          key: 'filterVulnerabilityWhites',
          width: 120,
          scopedSlots: { customRender: 'filterVulnerabilityWhites' },
        },
        {
          title: '过滤漏洞黑名单',
          i18nKey: 'BlockStrategy.FilterVulnerabilityBlacklist',
          dataIndex: 'filterVulnerabilityBlacks',
          key: 'filterVulnerabilityBlacks',
          width: 120,
          scopedSlots: { customRender: 'filterVulnerabilityBlacks' },
        },
        {
          title: '过滤license白名单',
          i18nKey: 'BlockStrategy.FilterLicenseWhitelist',
          dataIndex: 'filterLicenseWhites',
          key: 'filterLicenseWhites',
          width: 120,
          scopedSlots: { customRender: 'filterLicenseWhites' },
        },
        {
          title: '过滤license黑名单',
          i18nKey: 'BlockStrategy.FilterLicenseBlacklist',
          dataIndex: 'filterLicenseBlacks',
          key: 'filterLicenseBlacks',
          width: 120,
          scopedSlots: { customRender: 'filterLicenseBlacks' },
        },
        {
          title: '全量包名',
          i18nKey: 'BlockStrategy.FullPackageName',
          dataIndex: 'filterAllPackageName',
          key: 'filterAllPackageName',
          width: 100,
          scopedSlots: { customRender: 'filterAllPackageName' },
        },
        {
          title: '操作',
          i18nKey: 'BlockStrategy.Operate',
          dataIndex: 'operation',
          width: 180,
          scopedSlots: { customRender: 'operation' },
        },
      ],
      repositoriesList: [],
      selectedRepositoriesKeys: [],
      repositoriesLoading: false,
      repositoriesHasMore: false,
      repositoriesPage: 1,
      repositoriesLimit: 100000,
      repositoriesTotal: 0,
      packageNameBlockList: [],
      selectedPackageNameBlockKeys: [],
      packageNameBlockLoading: false,
      packageNameBlockHasMore: false,
      packageNameBlockPage: 1,
      packageNameBlockLimit: 100000,
      packageNameBlockTotal: 0,
      searchPackageName: undefined,
      licenseList: [],
      selectedLicenseKeys: [],
      licenseLoading: false,
      licenseHasMore: false,
      licensePage: 1,
      licenseLimit: 1000,
      licenseTotal: 0,
      searchLicense: undefined,
    }
  },
  components: {
  },
  computed: {
    i18nBlockStrategyColumns() {
      return this.blockStrategyColumns.map(column => {
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
      if (newval === "3") {
        this.initData()
      }
    },
  },
  mounted() { },
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js)
    },
    successMsg(message) {
      if (!message) {
        message = this.$t('BlockStrategy.OperationSuccessful');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    initData() {
      this.queryBlockStrategy()
      this.queryRepositories()
      this.queryPackageNameBlocks()
      this.queryLicenses()
    },
    queryBlockStrategy() {
      this.blockStrategyLoading = true
      queryBlockStrategy(this.blockStrategyQuery).then(res => {
        this.blockStrategyList = []
        if (res && res.data) {
          this.blockStrategyList = res.data.rows
          this.blockStrategyQuery.total = res.data.total
        }
      }).finally(() => {
        this.blockStrategyLoading = false
      })
    },
    getBlockStrategy(blockStrategy) {
      getBlockStrategy({ blockStrategyName: blockStrategy.blockStrategyName }).then(res => {
        if (res) {
          let white = "white", black = "black"
          this.blockStrategyForm.id = blockStrategy.id
          this.blockStrategyForm.blockStrategyName = res.blockStrategyName
          this.blockStrategyForm.vulnerabilityLevels = res.vulnerabilityLevels
          this.blockStrategyForm.filterAllPackageName = res.filterAllPackageName
          this.blockStrategyForm.filterAllLicense = res.filterAllLicense
          this.blockStrategyForm.filterVulnerability = []
          this.blockStrategyForm.filterLicense = []
          if (res.filterVulnerabilityWhites) {
            this.blockStrategyForm.filterVulnerability.push(white)
          }
          if (res.filterVulnerabilityBlacks) {
            this.blockStrategyForm.filterVulnerability.push(black)
          }
          if (res.filterLicenseWhites) {
            this.blockStrategyForm.filterLicense.push(white)
          }
          if (res.filterLicenseBlacks) {
            this.blockStrategyForm.filterLicense.push(black)
          }
          this.selectedRepositoriesKeys = res.repositories
          this.selectedPackageNameBlockKeys = res.packageNames
          this.selectedLicenseKeys = res.licenses
        }
      }).finally(() => {
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.blockStrategyQuery.page = pagination.current
      }
      this.queryBlockStrategy()
    },
    handheSearch() {
      this.blockStrategyQuery.page = 1
      this.queryBlockStrategy()
    },
    blockStrategyDelete(item) {
      deleteBlockStrategy({ blockStrategyName: item.blockStrategyName }).then(res => {
        this.successMsg(this.$t('BlockStrategy.OperationSuccessful'))
        this.queryBlockStrategy()
      }).catch((err) => {
        let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
        if (msg && msg.length > 0) {
          this.$notification.error({
            message: msg,
            description: ""
          })
        }
      })
    },
    showBlockStrategyInfo(item) {
      this.blockStrategyResetForm()
      if (item) {
        this.getBlockStrategy(item)
      }
      this.blockStrategyVisible = true
    },
    blockStrategyClose() {
      this.blockStrategyResetForm()
      this.blockStrategyVisible = false
    },
    blockStrategyFormSubmit() {
      this.$refs.blockStrategyFormRef.validate(valid => {
        if (valid) {
          let dataForm = Object.assign({}, this.blockStrategyForm)
          dataForm.repositories = this.selectedRepositoriesKeys
          dataForm.packageNames = this.selectedPackageNameBlockKeys
          dataForm.licenses = this.selectedLicenseKeys
          let white = "white", black = "black"
          if (this.blockStrategyForm.filterVulnerability) {
            if (this.blockStrategyForm.filterVulnerability.includes(white)) {
              dataForm.filterVulnerabilityWhites = true
            }
            if (this.blockStrategyForm.filterVulnerability.includes(black)) {
              dataForm.filterVulnerabilityBlacks = true
            }
          }
          if (this.blockStrategyForm.filterLicense) {
            if (this.blockStrategyForm.filterLicense.includes(white)) {
              dataForm.filterLicenseWhites = true
            }
            if (this.blockStrategyForm.filterLicense.includes(black)) {
              dataForm.filterLicenseBlacks = true
            }
          }
          if (dataForm.id) {
            updateBlockStrategy(dataForm).then(res => {
              this.successMsg(this.$t('BlockStrategy.OperationSuccessful'))
              this.queryBlockStrategy()
              this.blockStrategyClose()
            }).catch((err) => {
              let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
              if (msg && msg.length > 0) {
                this.$notification.error({
                  message: msg,
                  description: ""
                })
              }
            })
          } else {
            saveBlockStrategy(dataForm).then(res => {
              this.successMsg(this.$t('BlockStrategy.OperationSuccessful'))
              this.queryBlockStrategy()
              this.blockStrategyClose()
            }).catch((err) => {
              let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
              if (msg && msg.length > 0) {
                this.$notification.error({
                  message: msg,
                  description: ""
                })
              }
            })
          }
        }
      })
    },
    blockStrategyResetForm() {
      this.blockStrategyForm = {
        id: '',
        blockStrategyName: '',
        vulnerabilityLevels: [],
        filterVulnerability: [],
        filterLicense: [],
        filterAllPackageName: false,
        filterAllLicense: false,
        repositories: [],
        packageNames: [],
        licenses: [],
      }
      this.selectedRepositoriesKeys = []
      this.selectedPackageNameBlockKeys = []
      this.selectedLicenseKeys = []
      if (this.$refs.blockStrategyFormRef) {
        this.$refs.blockStrategyFormRef.resetFields()
      }
    },
    queryRepositories(isLoadMore) {
      if (!isLoadMore) {
        this.repositoriesPage = 1
        this.repositoriesList = []
        this.repositoriesHasMore = true
      }
      if (!this.repositoriesHasMore || this.repositoriesLoading) {
        return
      }
      this.repositoriesLoading = true;
      queryRepositories({ page: this.repositoriesPage, limit: this.repositoriesLimit }).then(res => {
        const newRepositories = res.data.rows.map((item) => {
          let temp = {}
          temp.key = `${item.storageId}:${item.id}`
          temp.title = temp.key
          temp.label = temp.key
          return temp
        })
        this.repositoriesList = [...this.repositoriesList, ...newRepositories]
        this.repositoriesTotal = res.data.total
        this.repositoriesHasMore = this.repositoriesList.length < this.repositoriesTotal
        this.repositoriesPage++
        this.selectedRepositoriesKeys.forEach(key => {
          if (!this.repositoriesList.some(use => use.key === key)) {
            let temp = {}
            temp.key = key
            temp.title = temp.key
            temp.label = temp.key
            this.repositoriesList.push(temp)
          }
        });
      }).catch(error => {
        console.error('Error fetching repositories:', error);
      }).finally(() => {
        this.repositoriesLoading = false;
      });
    },
    repositoriesOnChange(nextTargetKeys) {
      this.selectedRepositoriesKeys = nextTargetKeys
    },
    handleRepositoriesScroll(direction, e) {
      if (direction === 'left') {
        this.$nextTick(() => {
          const target = e.target
          const { scrollTop, clientHeight, scrollHeight } = target;
          if (scrollTop + clientHeight >= scrollHeight - 50) {
            if (!this.repositoriesLoading && this.repositoriesHasMore) {
              this.queryRepositories(true)
            }
          }
        })
      }
    },
    queryPackageNameBlocks(isLoadMore) {
      if (!isLoadMore) {
        this.packageNameBlockPage = 1
        this.packageNameBlockList = []
        this.packageNameBlockHasMore = true
      }
      if ((!this.packageNameBlockHasMore || this.packageNameBlockLoading) && this.searchPackageName === undefined) {
        return
      }
      this.packageNameBlockLoading = true;
      getPackageNameBlock({ page: this.packageNameBlockPage, limit: this.packageNameBlockLimit, packageName: this.searchPackageName }).then(res => {
        if (res.data.rows) {
          const newPackageNameBlocks = []
          const packageNameKey = []
          for (let item of res.data.rows) {
            let temp = {}
            temp.key = item.packageName
            temp.title = temp.key
            temp.label = temp.key
            if (!packageNameKey.includes(item.packageName)) {
              packageNameKey.push(item.packageName)
              newPackageNameBlocks.push(temp)
            }
          }
          if (this.packageNameBlockPage > 1) {
            this.packageNameBlockList = [...this.packageNameBlockList, ...newPackageNameBlocks]
          } else {
            this.packageNameBlockList = newPackageNameBlocks
          }
          this.packageNameBlockTotal = res.data.total
          this.packageNameBlockHasMore = this.packageNameBlockList.length < this.packageNameBlockTotal
          this.packageNameBlockPage++

          this.selectedPackageNameBlockKeys.forEach(key => {
            if (!this.packageNameBlockList.some(use => use.key === key)) {
              let temp = {}
              temp.key = key
              temp.title = temp.key
              temp.label = temp.key
              this.packageNameBlockList.push(temp)
            }
          })
        }
      }).catch(error => {
        console.error('Error fetching packageNameBlocks:', error);
      }).finally(() => {
        this.packageNameBlockLoading = false;
      });
    },
    packageNameBlockOnChange(nextTargetKeys) {
      this.selectedPackageNameBlockKeys = nextTargetKeys
    },
    handlePackageNameBlockSearch(dir, value) {
      if (dir === 'left') {
        this.searchPackageName = value
        this.packageNameBlockPage = 1
        this.queryPackageNameBlocks(true)
      }
    },
    handlePackageNameBlockScroll(direction, e) {
      if (direction === 'left') {
        this.$nextTick(() => {
          const target = e.target
          const { scrollTop, clientHeight, scrollHeight } = target;
          if (scrollTop + clientHeight >= scrollHeight - 50) {
            if (!this.packageNameBlockLoading && this.packageNameBlockHasMore) {
              this.queryPackageNameBlocks(true)
            }
          }
        })
      }
    },
    queryLicenses(isLoadMore) {
      if (!isLoadMore) {
        this.licensePage = 1
        this.licenseList = []
        this.licenseHasMore = true
      }
      if ((!this.licenseHasMore || this.licenseLoading) && this.searchLicense === undefined) {
        return
      }
      this.licenseLoading = true;
      getLicensesList({ page: this.licensePage, limit: this.licenseLimit, licenseId: this.searchLicense }).then(res => {
        const newLicenses = res.data.rows.map((item) => {
          let temp = {}
          temp.key = item.licenseId
          temp.title = temp.key
          temp.label = temp.key
          return temp
        })
        if (this.licensePage > 1) {
          this.licenseList = [...this.licenseList, ...newLicenses]
        } else {
          this.licenseList = newLicenses
        }
        this.licenseTotal = res.data.total
        this.licenseHasMore = this.licenseList.length < this.licenseTotal
        this.licensePage++

        this.selectedLicenseKeys.forEach(key => {
          if (!this.licenseList.some(use => use.key === key)) {
            let temp = {}
            temp.key = key
            temp.title = temp.key
            temp.label = temp.key
            this.licenseList.push(temp)
          }
        });
      }).catch(error => {
        console.error('Error fetching licenses:', error);
      }).finally(() => {
        this.licenseLoading = false;
      });
    },
    licenseOnChange(nextTargetKeys) {
      this.selectedLicenseKeys = nextTargetKeys
    },
    handleLicenseSearch(dir, value) {
      if (dir === 'left') {
        this.searchLicense = value
        this.licensePage = 1
        this.queryLicenses(true)
      }
    },
    handleLicenseScroll(direction, e) {
      if (direction === 'left') {
        this.$nextTick(() => {
          const target = e.target
          const { scrollTop, clientHeight, scrollHeight } = target;
          if (scrollTop + clientHeight >= scrollHeight - 50) {
            if (!this.licenseLoading && this.licenseHasMore) {
              this.queryLicenses(true)
            }
          }
        })
      }
    },
  },
};
</script>

<style lang="scss" scoped>
.v-search {
  max-width: 200px;
  width: 170px;
  min-width: 150px;
  margin-left: 5px;
  margin-bottom: 8px;
}
</style>
