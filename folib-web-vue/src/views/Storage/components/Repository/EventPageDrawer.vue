<template>
  <a-drawer
      placement="right"
      width="65%"
      title="事件"
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
        <a-tab-pane :key="1" tab="分发/晋级记录">
          <a-table :columns="tableColumns"
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
                  <template v-if="record.opsType === 2" v-for="(info, index) in JSON.parse(record.targetPath)">
                    分发节点{{index+1}}: {{info.dispatchClusterEnName}}
                    <template v-if="info.targetStorageId">&nbsp;&nbsp;存储空间: {{info.targetStorageId||'-'}}</template>
                    <template v-if="info.targetRepositoryId">&nbsp;&nbsp;仓库名称: {{info.targetRepositoryId||'-'}}</template>
                    <br/>
                  </template>
                </template>
                <a>
                  <p class="copy-p">
                    查看
                  </p>
                </a>
              </a-tooltip>
            </div>
            <div slot="opsType"
                 slot-scope="text, record">
              {{ opsTypeMap[record.opsType] || "未知操作" }}
            </div>
            <div slot="status"
                 slot-scope="text, record">
              {{ statusMap[record.status] || "未知状态" }}
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
                <a-popconfirm :title="(currentClickRecord && currentClickRecord.status === 2 ? '当前制品正在同步中，':'')+'确定要进行制品补偿吗'"
                              okType="danger"
                              ok-text="确定"
                              cancel-text="取消"
                              @confirm="">
                  <a-button type="link" v-if="record.status === 2 || record.status === 4" @click="clickRecord(record)"
                            size="small">
                    <span class="text-danger">补偿</span>
                  </a-button>
                </a-popconfirm>
              </div>
            </div>
          </a-table>
        </a-tab-pane>
        <a-tab-pane :key="2" tab="审计日志">
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
        1: "制品晋级",
        2: "制品分发"
      },
      statusMap: {
        1: "就绪",
        2: "同步中",
        3: "成功",
        4: "失败"
      },
      tableColumns: [
        {
          title: '制品同步编号',
          dataIndex: 'syncNo',
          key: 'syncNo',
          width: 100
        },
        {
          title: '源制品路径',
          dataIndex: 'sourcePath',
          key: 'sourcePath',
          width: 100
        },
        {
          title: '目标制品路径信息',
          dataIndex: 'targetPath',
          key: 'targetPath',
          width: 80,
          scopedSlots: {customRender: 'targetPath'}
        },
        {
          title: '制品操作',
          dataIndex: 'opsType',
          key: 'opsType',
          width: 100,
          scopedSlots: {customRender: 'opsType'}
        },
        {
          title: '同步状态',
          dataIndex: 'status',
          key: 'status',
          width: 100,
          scopedSlots: {customRender: 'status'}
        },
        {
          title: '同步进度',
          dataIndex: 'syncProgress',
          key: 'syncProgress',
          width: 100,
          scopedSlots: {customRender: 'syncProgress'}
        },
        {
          title: '状态时间',
          dataIndex: 'createTime',
          key: 'createTime',
          width: 100,
          scopedSlots: {customRender: 'createTime'}
        },
        {
          title: '操作',
          dataIndex: 'operation',
          width: 100,
          scopedSlots: {customRender: 'operation'}
        }
      ],
      recordList: [],
      dataFilter: {
        storageId: "",
        repositoryId: "",
        pageNumber: 1,
        pageSize: 2,
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
        }, 1000);
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
    },
  },
};
</script>