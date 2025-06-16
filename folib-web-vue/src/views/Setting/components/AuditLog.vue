<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="18" class="mb-24">
        <span class="mr-10 pl-5">{{ $t('Setting.auditModule') }}</span>
        <a-select
            style="width: 120px"
            v-model="queryParam.moduleValue"
            :placeholder="$t('Setting.auditModule')"
            :options="auditModules"
            @change="handleChange"
        />
        <span class="ml-10 mr-5 pl-5">{{ $t('Setting.auditEvent') }}</span>
        <a-select
            style="width: 150px"
            v-model="queryParam.eventValue"
            :placeholder="$t('Setting.auditEvent')"
            :options="auditEvents"
        ></a-select>
        <span class="mr-10 pl-10">{{ $t('Setting.eventTime') }}</span>
        <a-range-picker
            v-model="dateRange"
            :format="dateFormat"
            @change="onDateChange"
            style="width: 300px"
            :placeholder="[$t('Setting.startTime'), $t('Setting.endTime')]"
        />
      </a-col>
      <a-col :span="6" class="mb-24 text-right">
        <a-button type="primary" style="width: 80px" @click="searchLog">搜索</a-button>
        <a-button class="ml-10" style="width: 80px" @click="resetForm">重置</a-button>
      </a-col>
    </a-row>
    <a-card :bordered="false" class="header-solid mb-24" :bodyStyle="{padding: 0}">
      <a-table class="mt-20"
               :columns="i18nColumns"
               :data-source="auditLogs"
               :pagination="{pageSize:queryParam.pageSize,total,current:this.queryParam.pageNumber,}"
               @change="handleTableChange"
               rowKey="id">
        <template slot="detail" slot-scope="record">
          <a-button type="link" @click="showDrawer(record)">查看详情</a-button>
        </template>
      </a-table>
    </a-card>

    <a-drawer
        :title="$t('Setting.detail')"
        :visible="drawerOpen"
        placement=right
        :closable="false"
        @close="onClose" :width="500">
        <a-descriptions
            :column="1"
            title="事件详情"
            :size="'middle'"
        >
          <a-descriptions-item label="事件模块">{{logDetail.moduleName}}</a-descriptions-item>
          <a-descriptions-item label="事件名称">{{logDetail.eventName}}</a-descriptions-item>
          <a-descriptions-item label="事件对象">{{logDetail.target}}</a-descriptions-item>
          <a-descriptions-item label="事件时间">{{logDetail.createTime}}</a-descriptions-item>
          <a-descriptions-item label="操作用户">{{logDetail.username}}</a-descriptions-item>
          <a-descriptions-item label="操作结果">
            <a-tag v-if="logDetail.result===1" color="green">成功</a-tag>
            <a-tag v-if="logDetail.result===0" color="red">失败</a-tag>
          </a-descriptions-item>
        </a-descriptions>

      <a-descriptions
          bordered
          :column="1"
          title="事件记录"
          :size="'middle'"
      >
        <a-descriptions-item label="request">
          <pre>{{ formatJson(logDetail.request)}}</pre>
        </a-descriptions-item>
        <a-descriptions-item label="response">
          <pre>{{formatJson(logDetail.response)}}</pre>
        </a-descriptions-item>
      </a-descriptions>
    </a-drawer>

  </div>

</template>

<script>

import {getAuditLogList, getEventByModule, getModuleList} from "@/api/audit"


export default {
  data() {
    return {
      queryParam: {
        'moduleValue': '',
        'eventValue': '',
        'pageNumber': 1,
        'pageSize': 10,
        'fromDate': '',
        'toDate': ''
      },
      columns: [
        {
          title: '事件模块',
          i18nKey: 'Setting.auditModule',
          dataIndex: 'moduleName',
          key: 'moduleName',
        },
        {
          title: '事件名称',
          i18nKey: 'Setting.auditEvent',
          dataIndex: 'eventName',
          key: 'eventName',
        },
        {
          title: '事件目标',
          i18nKey: 'Setting.target',
          dataIndex: 'target',
          key: 'target',
        },
        {
          title: '操作时间',
          i18nKey: 'Setting.createTime',
          dataIndex: 'createTime',
          key: 'createTime',
        },
        {
          title: '操作人',
          i18nKey: 'Setting.operator',
          dataIndex: 'username',
          key: 'username',
        },
        {
          title: '操作结果',
          i18nKey: 'Setting.result',
          dataIndex: 'result',
          key: 'result',
          customRender: (text) => {
            return text === 1 ? '成功' : '失败';
          }
        },
        {
          title: '操作详情',
          i18nKey: 'Setting.detail',
          key: 'detail',
          scopedSlots: {customRender: "detail"},
        }
      ],
      total: 0,
      drawerOpen: false,
      auditEvents: [],
      auditModules: [],
      auditLogs: [],
      dateRange: [],
      dateFormat: 'YYYY-MM-DD',
      pageSize: 10,
      logDetail:{},
    }
  },
  methods: {
    handleChange() {
      this.queryParam.eventValue = "";
      getEventByModule(this.queryParam.moduleValue).then(
          res => {
            this.auditEvents = res.map(item => ({
              label: item.eventName,  // 显示给用户的名称
              value: item.eventValue     // 选项的实际值
            }));
          }
      )
    },
    handleTableChange(pagination) {
      this.queryParam.pageNumber = pagination.current;
      this.searchLog();
    },
    onDateChange() {
      if (this.dateRange && this.dateRange.length === 2) {
        this.queryParam.fromDate = this.dateRange[0].format("YYYY-MM-DD") + " 00:00:00";
        this.queryParam.toDate = this.dateRange[1].format("YYYY-MM-DD") + " 23:59:59";
      } else {
        this.queryParam.fromDate = null;
        this.queryParam.toDate = null;
      }
    },
    getAllModules() {
      getModuleList().then(res => {
        this.auditModules = res.map(item => ({
          label: item.moduleName,  // 显示给用户的名称
          value: item.moduleValue     // 选项的实际值
        }));
      })
    },
    resetForm() {
      this.dateRange = [];
      this.queryParam.eventValue = null;
      this.queryParam.moduleValue = null;
    },
    searchLog() {
      getAuditLogList(this.queryParam).then(res => {
        this.auditLogs = res.data.rows;
        this.total = res.data.total;
      })
    },
    showDrawer(date) {
      this.drawerOpen = true;
      this.logDetail={...date};
    },
    onClose(){
      this.drawerOpen = false;
      this.logDetail={};
    },
    formatJson(json){
      if(json){
        return  JSON.stringify(JSON.parse(json), null, 2);
      }else {
        return json;
      }

    }
  },
  mounted() {
    this.getAllModules();
    this.searchLog();
  },
  computed: {
    i18nColumns() {
      return this.columns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    }
  },
}

</script>

<style lang="scss" scoped>
.table-avatar-info {
  display: flex;
  align-items: center;
}

.table-avatar-info .ant-avatar {
  margin-right: 8px;
}

// Using vuejs "Deep Selectors"
.table-avatar-info::v-deep .ant-avatar-string {
  font-size: 12px;
}

.btn-status::v-deep .anticon {
  line-height: 0;
}
pre {
  padding: 10px;
  border-radius: 5px;
  overflow-x: auto;
}
</style>