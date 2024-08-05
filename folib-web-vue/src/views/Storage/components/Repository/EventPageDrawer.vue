<template>
  <a-drawer
      placement="right"
      width="65%"
      :title="$t('Repository.EventRecord')"
      :visible="eventPageVisible"
      @close="eventDrawerClose"
      :zIndex="100"
  >
    <a-card
        :bordered="false"
        class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
        :headStyle="{ paddingRight: 0 }"
    >
      <a-tabs
          class="tabs-sliding"
          :default-active-key="1"
          :activeKey="settingTabActiveKey"
          @change="settingTabChange($event)"
      >
        <a-tab-pane :key="1" :tab="$t('Repository.Distribution')+'/'+$t('Repository.promotionRecords')">
          <a-table :columns="i18nTableColumns"
                   @change="tableChange"
                   :pagination="{pageSize: dataFilter.pageSize, current: dataFilter.pageNumber, total: dataFilter.total, showLessItems: true}"
                   :scroll="{ x: true }"
                   :data-source="recordList"
                   :row-key="(r, i) => i.toString()">
            <div slot="targetPath"
                 slot-scope="text, record">
              <a-tooltip>
                <template slot="title">
                  <template v-if="record.opsType === 1">
                    {{record.targetPath}}
                  </template>
                  <template v-if="record.opsType === 2">
                    <template v-for="(info, index) in JSON.parse(record.targetPath)">
                      {{ $t('Repository.DistributionNode') }}{{index+1}}: {{info.dispatchClusterEnName}}
                      <template v-if="info.targetStorageId">&nbsp;&nbsp;{{ $t('Repository.StorageSpace') }}: {{info.targetStorageId||'-'}}</template>
                      <template v-if="info.targetRepositoryId">&nbsp;&nbsp;{{ $t('Repository.WarehouseName') }}: {{info.targetRepositoryId||'-'}}</template>
                      <br/>
                    </template>
                  </template>
                </template>
                <a>
                  <p class="copy-p">
                    {{ $t('Repository.CheckOut') }}
                  </p>
                </a>
              </a-tooltip>
            </div>
            <div slot="failedReason"
                 slot-scope="text, record">
              <template v-if="record.failedReason">
                <a-tooltip>
                  <template slot="title">
                      {{record.failedReason}}
                    </template>
                  <a>
                    <p class="copy-p">
                      {{ $t('Repository.CheckOut') }}
                    </p>
                  </a>
                </a-tooltip>
              </template>
              <template v-else>
                -
              </template>
            </div>
            <div slot="opsType"
                 slot-scope="text, record">
              {{ $t('Repository.' + opsTypeMap[record.opsType]) || $t('Repository.UnknownOperation') }}
            </div>
            <div slot="status"
                 slot-scope="text, record">
              {{ $t('Repository.' + statusMap[record.status]) || $t('Repository.UnknownState') }}
            </div>
            <div slot="slaveRecordCleared"
                 slot-scope="text, record">
              {{ record.slaveRecordCleared ? $t('Repository.Cleared'):$t('Repository.NotCleared') }}
            </div>
            <div slot="syncProgress"
                 slot-scope="text, record">
              <template v-if="record.syncProgress && record.syncProgress > 0">
                {{ (record.syncProgress * 100).toFixed(2) }} %
              </template>
              <template v-else>
                0.00%
              </template>
            </div>
            <div slot="operation"
                 slot-scope="text, record">
              <div class="col-action">
                <a-popconfirm :title="(currentClickRecord && currentClickRecord.status === 2 ? $t('Repository.CurrentProductIsSynchronizing'):'')+$t('Repository.SureMakeProductCompensation')"
                              okType="danger"
                              :ok-text="$t('Repository.Confirm')"
                              :cancel-text="$t('Repository.Cancel')"
                              @confirm="clickRecord(record)">
                  <a-button type="link" v-if="record.status === 4"
                            size="small">
                    <span class="text-danger">{{ $t('Repository.Compensation') }}</span>
                  </a-button>
                </a-popconfirm>
              </div>
            </div>
          </a-table>
        </a-tab-pane>
        <a-tab-pane :key="2" :tab="$t('Repository.AuditLog')">
        </a-tab-pane>
      </a-tabs>
    </a-card>
  </a-drawer>
</template>
<script>
import {} from "@/api/folib"
import Permission from '../Permission/index.vue'
import CronTask from "../Cron/index.vue"
import UnionRepository from "../UnionRepository/index.vue"
import {getArtifactDispatchConfig, getArtifactSyncRecordPage} from "@/api/settings";
import {retryArtifactDispatch, retryNodeOption} from "@/api/artifact";

export default {
  props: {
    folibRepository: {
      type: Object,
      default: {},
    },
    eventPageVisible: {
      type: Boolean,
      default: false,
    },
  },
  data() {
    return {
      intervalId: undefined,
      settingTabActiveKey: 1,
      opsTypeMap: {
        1: "ProductPromotion",
        2: "DistributionOfProducts"
      },
      statusMap: {
        1: "Ready",
        2: "InSync",
        3: "Success",
        4: "Failure"
      },
      tableColumns: [
        {
          title: '制品同步编号',
          i18nKey: 'Repository.ProductSynchronousNumber',
          dataIndex: 'syncNo',
          key: 'syncNo',
          width: 180
        },
        {
          title: '源制品路径',
          i18nKey: 'Repository.SourceProductPath',
          dataIndex: 'sourcePath',
          key: 'sourcePath',
          width: 120
        },
        {
          title: '目标制品路径信息',
          i18nKey: 'Repository.TargetProductPathInformation',
          dataIndex: 'targetPath',
          key: 'targetPath',
          width: 160,
          scopedSlots: {customRender: 'targetPath'}
        },
        {
          title: '制品操作',
          i18nKey: 'Repository.ProductOperation',
          dataIndex: 'opsType',
          key: 'opsType',
          width: 120,
          scopedSlots: {customRender: 'opsType'}
        },
        {
          title: '同步状态',
          i18nKey: 'Repository.SynchronousState',
          dataIndex: 'status',
          key: 'status',
          width: 120,
          scopedSlots: {customRender: 'status'}
        },
        {
          title: '从记录状态',
          i18nKey: 'Repository.FromTheRecordState',
          dataIndex: 'slaveRecordCleared',
          key: 'slaveRecordCleared',
          width: 120,
          scopedSlots: {customRender: 'slaveRecordCleared'}
        },
        {
          title: '同步进度',
          i18nKey: 'Repository.SynchronizingProgress',
          dataIndex: 'syncProgress',
          key: 'syncProgress',
          width: 120,
          scopedSlots: {customRender: 'syncProgress'}
        },
        {
          title: '失败原因',
          i18nKey: 'Repository.CauseOfFailure',
          dataIndex: 'failedReason',
          key: 'failedReason',
          width: 100,
          scopedSlots: {customRender: 'failedReason'}
        },
        {
          title: '创建时间',
          i18nKey: 'Repository.CreationTime',
          dataIndex: 'createTime',
          key: 'createTime',
          width: 120,
          scopedSlots: {customRender: 'createTime'}
        },
        {
          title: '操作',
          i18nKey: 'Repository.Operation',
          dataIndex: 'operation',
          width: 110,
          scopedSlots: {customRender: 'operation'}
        }
      ],
      recordList: [],
      dataFilter: {
        storageId: "",
        repositoryId: "",
        pageNumber: 1,
        pageSize: 20,
        total: 0
      },
      currentClickRecord: undefined
    }
  },
  components: {
    CronTask,
    Permission,
    UnionRepository,
  },
  computed: {
    i18nTableColumns() {
      return this.tableColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  created() {
    this.dataFilter.storageId = this.folibRepository.storageId
    this.dataFilter.repositoryId = this.folibRepository.id
  },
  mounted() {
  },
  watch: {
    eventPageVisible: function (val) {
      if (val) {
        this.getArtifactSyncRecordPage()
        this.intervalId = setInterval(() => {
          this.getArtifactSyncRecordPage()
        }, 3000);
      } else {
        if (this.intervalId) {
          clearInterval(this.intervalId);
          this.intervalId = undefined
        }
      }
    }
  },
  methods: {
    settingTabChange(activeKey) {
      this.settingTabActiveKey = activeKey
    },
    eventDrawerClose() {
      this.$emit('eventDrawerClose')
    },
    getArtifactSyncRecordPage() {
      getArtifactSyncRecordPage(this.dataFilter)
          .then(res => {
            this.recordList = res.data.rows
            this.dataFilter.total = res.data.total
          })
    },
    tableChange(pagination, filters, sorter) {
      this.dataFilter.pageNumber = pagination.current
      this.dataFilter.pageSize = pagination.pageSize
      this.getArtifactSyncRecordPage()
    },
    clickRecord(v) {
            this.currentClickRecord = v
            let sycnNo = this.currentClickRecord.syncNo;
            //1：制品晋级；2：制品分发
            let opsType = this.currentClickRecord.opsType;
            if (opsType === 1) {
                this.vulnerabilityTableLoading = true
                retryNodeOption(sycnNo).then(res => {
                    this.$message.success(this.$t("Storage.OperationSuccessful"));
                    this.getArtifactSyncRecordPage();
                }).finally(() => {
                    this.vulnerabilityTableLoading = false
                });
            } else if (opsType === 2) {
                const jsonArrayString = JSON.parse(this.currentClickRecord.targetPath);
                let type = jsonArrayString[0].artifactoryRepositoryType;
                retryArtifactDispatch(sycnNo, type).then(res => {
                    this.$message.success(this.$t("Storage.OperationSuccessful"));
                    this.getArtifactSyncRecordPage();
                }).finally(() => {
                    this.vulnerabilityTableLoading = false
                });
            }
    },
  },
};
</script>
