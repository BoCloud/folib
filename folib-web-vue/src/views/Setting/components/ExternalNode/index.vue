<template>
  <a-card class="header-solid block">
    <div class="mx-25 mb-50">
      <a-col :span="24" class="text-right">
        <a-tooltip @click="externalNodeHandler(1)">
          <template slot="title">新增</template>
          <a-icon type="plus-circle" theme="filled" class="cursor-pointer"
            :style="{ fontSize: '28px', color: '#1890FF' }" />
        </a-tooltip>
      </a-col>
    </div>
    <a-table :columns="externalNodeColumns" :data-source="externalNodeList" :scroll="{ x: true }"
      @change="handleChangeTable"
      :loading="loading"
      :pagination="{ pageSize: externalNodeQuery.limit, current: externalNodeQuery.page, total: externalNodeQuery.total, showLessItems: true }"
      :row-key="(r, i) => i.toString()">
      <div slot="operation" slot-scope="text, record">
        <div class="col-action">
          <a-popconfirm title="确定要删除吗？" okType="danger" ok-text="确定" cancel-text="取消"
            @confirm="externalNodeHandlerDelete(record)">
            <a-button type="link" size="small">
              <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                  d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                  fill="#111827" />
              </svg>
              <span class="text-danger">DELETE</span>
            </a-button>
          </a-popconfirm>
          <a-button type="link" size="small" @click="externalNodeHandler(2, record)">
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
    <ExternalNodeModal v-if="showModelVisible" :modelVisible="showModelVisible"
      :handlerExternalNodeType="handlerExternalNodeType" :externalNode="externalNode"
      @externalNodeHandlerCancel="externalNodeHandlerCancel" @externalNodeReflesh="externalNodeReflesh" />
  </a-card>
</template>
<script>
import { getExternalNodeList, getExternalNodeInfo, deleteExternalNode } from "@/api/externalNode"
import ExternalNodeModal from './modal.vue'
export default {
  props: [

  ],
  components: {
    ExternalNodeModal,
  },
  data() {
    return {
      externalNodeColumns: [
        {
          title: '外部节点名称',
          dataIndex: 'nodeName',
          key: 'nodeName',
          width: 150,
          scopedSlots: { customRender: 'nodeName' },
        },
        {
          title: '外部节点类型',
          dataIndex: 'type',
          key: 'type',
          width: 100,
          scopedSlots: { customRender: 'type' },
        },
        {
          title: '外部节点地址',
          dataIndex: 'address',
          key: 'address',
          width: 200,
          scopedSlots: { customRender: 'address' },
        },
        {
          title: '用户名',
          dataIndex: 'username',
          key: 'username',
          width: 100,
          scopedSlots: { customRender: 'username' },
        },
        {
          title: '创建时间',
          dataIndex: 'createTime',
          key: 'createTime',
          width: 200,
          scopedSlots: { customRender: 'createTime' },
        },
        {
          title: '操作',
          dataIndex: 'operation',
          width: 100,
          scopedSlots: { customRender: 'operation' },
        },
      ],
      loading: false,
      externalNodeList: [],
      externalNodeQuery: {
        page: 1,
        limit: 10,
        total: 0,
        nodeName: undefined,
      },
      showModelVisible: false,
      handlerExternalNodeType: 1,
      externalNode: undefined,
    }
  },
  created() {
    this.getExternalNodeList()
  },
  mounted() { },
  methods: {
    successMsg(message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification["success"]({
        message: message,
      })
    },
    getExternalNodeList() {
      this.loading = true
      getExternalNodeList(this.externalNodeQuery).then(res => {
        if (res) {
          this.externalNodeList = res.data.rows
          this.externalNodeQuery.total = res.data.total
        }
      }).finally(() => { 
        this.loading = false
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.externalNodeQuery.page = pagination.current
      }
      this.getExternalNodeList()
    },
    externalNodeTableReflesh() {
      this.externalNodeQuery.page = 1
      this.getExternalNodeList()
    },
    externalNodeHandler(type, record) {
      this.externalNode = undefined
      this.handlerExternalNodeType = type
      if (record) {
        this.externalNode = record
      }
      this.showModelVisible = true
    },
    externalNodeHandlerCancel() {
      this.showModelVisible = false
    },
    externalNodeReflesh() {
      this.getExternalNodeList()
      this.showModelVisible = false
    },
    externalNodeHandlerDelete(record) {
      deleteExternalNode(record.id).then((res) => {
        this.successMsg("删除外部节点成功")
        this.externalNodeTableReflesh()
      }).catch((err) => {
        this.$notification["error"]({
          message: err.response.data.error,
        })
      }).finally(() => { })
    }
  },
}
</script>