<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }">
          <template #title>
            <h6 class="font-semibold m-0">Webhooks</h6>
          </template>
          <a slot="extra" @click="showWebhookInfo(null)">
            <a-tooltip>
              <template slot="title">新增</template>
              <a-icon type="plus-circle" theme="filled" class="cursor-pointer mr-10"
                :style="{ fontSize: '28px', color: '#1890FF' }" />
            </a-tooltip>
          </a>
          <a-row :gutter="[24, 24]">
            <a-col :span="24" v-for="(item, index) in webhooks" :key="index">
              <a-card :bordered="false" class="card-billing-info">
                <div class="col-info">
                  <a-descriptions :title="item.url" :column="1">
                    <a-descriptions-item label="访问令牌">
                      {{ item.accessToken }}
                    </a-descriptions-item>
                    <a-descriptions-item label="触发事件">
                      <div v-if="item.events">
                        <span v-for="(se, sei) in events" :key="sei">
                          <span v-if="item.events.includes(se.value)" :class="sei==0?'':'ml-10'">{{ "[" + se.label +  "]" }}</span>
                        </span>
                      </div>
                    </a-descriptions-item>
                    <!-- <a-descriptions-item label="SSL验证">
                      <a-tag color="#87d068" v-if="item.ssl">
                        已启用
                      </a-tag>
                      <a-tag color="#f50" v-else>
                        已禁用
                      </a-tag>
                    </a-descriptions-item> -->
                  </a-descriptions>
                </div>
                <div class="col-action">
                  <a-popconfirm title="确定要删除吗？" okType="danger" ok-text="确定" cancel-text="取消"
                    @confirm="deleteWebhook(item)">
                    <a-button type="link" size="small">
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                          d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                          fill="#111827" />
                      </svg>
                      <span class="text-danger">DELETE</span>
                    </a-button>
                  </a-popconfirm>
                  <a-button type="link" size="small" @click="showWebhookInfo(item)">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-muted"
                        d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                        fill="#111827" />
                      <path class="fill-muted"
                        d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827" />
                    </svg>
                    <span class="text-dark">EDIT</span>
                  </a-button>
                  <a-button type="link" size="small" @click="showWebhookLog(item)">
                    <a-icon type="eye" theme="filled" />
                    <span class="text-dark">VIEW</span>
                  </a-button>
                  <a-dropdown class="ml-5">
                    <a class="ant-dropdown-link webhook-test">
                      TEST <a-icon type="down" />
                    </a>
                    <a-menu slot="overlay" @click="doTestWebhook($event, item)">
                      <a-menu-item v-for="(event, ei) in item.eventList" :key="ei">
                        <a href="javascript:;">
                          {{ event.label }}
                        </a>
                      </a-menu-item>
                    </a-menu>
                  </a-dropdown>
                </div>
              </a-card>
            </a-col>
          </a-row>
        </a-card>
      </a-col>
    </a-row>
    <a-drawer
      placement="right"
      width="45%"
      title="webhook信息"
      :visible="webhookVisible"
      @close="webhookDrawerClose"
      :zIndex="100"
    >
      <a-card
        :bordered="false"
        class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
        :headStyle="{ paddingRight: 0 }"
      >
        <a-form-model
          layout="horizontal"
          ref="webhookForm"
          :model="webhookForm"
          :rules="webhookRules"
          :hideRequiredMark="false"
        >
          <a-form-model-item
            class="mb-10"
            label="URL"
            :colon="false"
            prop="url"
          >
            <a-input
              placeholder="请输入url"
              v-model="webhookForm.url"
            />
          </a-form-model-item>
          <a-form-model-item
            class="mb-10"
            label="访问令牌 Header X-Folibrary-Token"
            :colon="false"
            prop="accessToken"
          >
            <a-input
              placeholder="请输入访问令牌"
              v-model="webhookForm.accessToken"
            />
          </a-form-model-item>
          <a-form-model-item class="mb-10"
            label="触发事件"
            :colon="false"
            prop="events">
            <a-checkbox-group v-model="webhookForm.events">
              <a-row type="flex" :gutter="24">
                <a-col :span="24" class="mb-24" v-for="(item, index) in events" :key="index">
                  <a-checkbox :value="item.value" name="events" >
                    {{ item.label }}
                  </a-checkbox>
                  <a-col :span="24" class="mt-5 ml-10">
                    {{ item.remark }}
                  </a-col>
                </a-col>
              </a-row>
            </a-checkbox-group>
          </a-form-model-item>
          <!-- <a-form-model-item
            class="mb-10"
            label="SSL验证"
            :colon="false"
            prop="accessToken"
          >
            <a-switch
              v-model="webhookForm.ssl"
            />
          </a-form-model-item> -->
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="webhookFormSubmit">
              保存
            </a-button>
            <a-button class="ml-10" @click="webhookDrawerClose">
              取消
            </a-button>
          </a-form-model-item>
        </a-form-model>
      </a-card>
    </a-drawer>
    <a-drawer
      placement="right"
      width="80%"
      title="Webhook记录"
      :visible="webhookLogVisible"
      @close="webhookLogDrawerClose"
      :zIndex="100"
    >
      <a-card
        :bordered="false"
        class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
        :headStyle="{ paddingRight: 0 }"
      >
        <a-table :columns="webhookLogColumns" :data-source="webhookLogList">
          <div slot="responseStatus" slot-scope="responseStatus">
            <a-tag color="#f50" v-if="responseStatus != 200">
              错误
            </a-tag>
            <a-tag color="#87d068" v-else>
              {{responseStatus}}
            </a-tag>
          </div>
          <div slot="eventType" slot-scope="eventType">
            {{ queryEvent(eventType) }}
          </div>
          <div slot="operation" slot-scope="text, record">
            <div class="col-action">
              <a-popconfirm title="确定要删除吗？" okType="danger" ok-text="确定" cancel-text="取消"
                @confirm="webhookLogDelete(record)">
                <a-button type="link" size="small">
                  <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                      d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                      fill="#111827" />
                  </svg>
                  <span class="text-danger">DELETE</span>
                </a-button>
              </a-popconfirm>
              <a-button type="link" size="small" @click="showWebhookLogInfo(record)">
                <a-icon type="eye" theme="filled" :style="{ color: '#656464' }" />
                <span class="text-dark">VIEW</span>
              </a-button>
            </div>
          </div>
        </a-table>
      </a-card>
    </a-drawer>
    <a-drawer
      placement="right"
      width="50%"
      title="Webhook记录详情"
      :visible="webhookLogInfoVisible"
      @close="webhookLogInfoDrawerClose"
      :zIndex="100"
    >
      <a-card
        :bordered="false"
        class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
        :headStyle="{ paddingRight: 0 }"
      >
        <template #title>
          <h6 class="font-semibold m-0">
            <span>{{ webhookLogInfo.method }}</span>
            <span class="ml-10">{{ webhookLogInfo.url }}</span>
          </h6>
          <span class="ml-0">
            <a-tag color="green">
              {{ queryEvent(webhookLogInfo.eventType) }}
            </a-tag>
            在{{ webhookLogInfo.completionTime}}秒内完成
          </span>
        </template>
        <a-button type="link" slot="extra">
        
        </a-button>
        <p class="text-dark ml-5">
          <a-tag color="red">
            <a-icon type="exclamation-circle"/>
            <span class="ml-10">发送此 webhook 时发生内部错误。</span>
            <div v-if="webhookLogInfo.remark" class="ml-20"> 
              错误：{{ webhookLogInfo.remark }}
            </div>
          </a-tag>
        </p>
        <hr class="my-25">
        <a-descriptions title="响应" :column="1">
          <a-descriptions-item label="">
            <prism-editor
              class="metadata-prism-editor"
              v-if="webhookLogInfo.response"
              v-model="webhookLogInfo.response"
              :highlight="highlighterHandle"
              :line-numbers="true"
              :readonly="true"
            >
            </prism-editor>
          </a-descriptions-item>
        </a-descriptions>
        <hr class="my-25">
        <a-descriptions title="Headers" :column="1">
          <a-descriptions-item :label="key" v-for="(value, key, index) in webhookLogInfo.responseHeaders" :key="index">
            {{ value }}
          </a-descriptions-item>
        </a-descriptions>
        <hr class="my-25">
        <a-descriptions title="请求" :column="1">
          <a-descriptions-item label="">
            <prism-editor
              class="metadata-prism-editor"
              v-if="webhookLogInfo.request"
              v-model="webhookLogInfo.request"
              :highlight="highlighterHandle"
              :line-numbers="true"
              :readonly="true"
            >
            </prism-editor>
          </a-descriptions-item>
        </a-descriptions>
        <hr class="my-25">
        <a-descriptions title="Headers" :column="1">
          <a-descriptions-item :label="key" v-for="(value, key, index) in webhookLogInfo.requestHeaders" :key="index">
            {{ value }}
          </a-descriptions-item>
        </a-descriptions>
      </a-card>
    </a-drawer>
  </div>
</template>
<script>
import {
  getWebhooks,
  saveWebhook,
  updateWebhook,
  deleteWebhook,
  queryWebhookLog,
  testWebhook,
  deleteWebhookLog,
} from "@/api/webhook"
import { PrismEditor } from "vue-prism-editor"
import "vue-prism-editor/dist/prismeditor.min.css"
import { highlight, languages } from "prismjs/components/prism-core"
import "prismjs/components/prism-clike"
import "prismjs/components/prism-javascript"
import "prismjs/themes/prism-tomorrow.css"

export default {
  props: { 
    activeKey: {
			type: String,
			default: "",
		},
	},
  data() {
    const acceptUrlValidator = (rule, value, callBack) => {
      let url = /^(?:http(s)?:\/\/)?[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/[\]@!\$&'\*\+,;=.]+$/;
      if (value) {
        if (!url.test(value)) {
          callBack("请输入正确的URL");
        } else {
          callBack();
        }
      } else {
        callBack("请输入正确的URL");
      }
    }
    return {
      webhooks: [],
      webhookVisible: false,
      events:[ 
        {
          label: '制品上传',
          value: 'EVENT_ARTIFACT_FILE_STORED',
          remark: '上传制品时触发 URL'
        },
        {
          label: '制品更新',
          value: 'EVENT_ARTIFACT_FILE_UPDATED',
          remark: '再次上传同一制品时触发 URL'
        },
        {
          label: '制品下载',
          value: 'EVENT_ARTIFACT_FILE_DOWNLOADED',
          remark: '下载制品时触发 URL'
        },
        {
          label: '制品删除',
          value: 'EVENT_ARTIFACT_PATH_DELETED',
          remark: '删除制品时触发 URL'
        },
        {
          label: '目录删除',
          value: 'EVENT_ARTIFACT_DIRECTORY_PATH_DELETED',
          remark: '删除制品目录时触发 URL'
        },
        {
          label: '安全阻断',
          value: 'EVENT_ARTIFACT_FILE_DOWNLOAD_BLOCKED',
          remark: '下载被安全策略阻断时触发 URL'
        },
      ],
      webhookForm: {
        uuid: '',
        url: '',
        accessToken: '',
        events: [],
        ssl: false,
      },
      webhookRules: {
        url: [
          { required: true, trigger: ['blur', 'change'], validator: acceptUrlValidator },
          { min: 10, max: 255, message: 'url长度在10到255个字符', trigger: 'blur' },
        ],
        accessToken: [
          { min: 1, max: 255, message: '访问令牌长度在1到255 个字符', trigger: 'blur' },
        ],
        events: [
          { required: true, message: '请选择触发事件', trigger: ['blur', 'change'] },
        ],
      },
      webhookLogVisible: false,
      webhookLogColumns: [
        {
          title: '存储空间',
          dataIndex: 'storageId',
          key: 'storageId',
          width: 150,
          scopedSlots: { customRender: 'storageId' },
        },
        {
          title: '仓库名称',
          dataIndex: 'repositoryId',
          key: 'repositoryId',
          width: 150,
          scopedSlots: { customRender: 'repositoryId' },
        },
        {
          title: '制品路径',
          dataIndex: 'artifactPath',
          key: 'artifactPath',
          // width: 100,
          scopedSlots: { customRender: 'artifactPath' },
        },
        {
          title: '状态',
          dataIndex: 'responseStatus',
          key: 'responseStatus',
          width: 100,
          scopedSlots: { customRender: 'responseStatus' },
        },
        {
          title: '触发事件',
          dataIndex: 'eventType',
          key: 'eventType',
          width: 150,
          scopedSlots: { customRender: 'eventType' },
        },
        // {
        //   title: '请求耗时（秒）',
        //   dataIndex: 'completionTime',
        //   key: 'completionTime',
        //   width: 100,
        //   scopedSlots: { customRender: 'completionTime' },
        // },
        {
          title: '请求时间',
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
      webhookLogList: [],
      webhookLogInfoVisible: false,
      webhookLogInfo: {
        accessToken:  "",
        artifactPath: "",
        completionTime: "",
        createTime: "",
        eventType: "",
        id: 0,
        method: "",
        remark: "",
        repositoryId: "",
        request: null,
        requestHeaders: null,
        response: null,
        responseHeaders: null,
        responseStatus: "",
        storageId: "",
        url: "",
      }
    }
  },
  components: {
    PrismEditor,
  },
  computed: {

  },
  created() {
    this.initData()
  },
	watch: {
    'activeKey' : function (newval) {
      if (newval === "7") {
        this.initData()
      }
    },
  },
  mounted() {},
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js)
    },
    successMsg(message) {
      if (!message) {
        message = "操作成功";
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    queryEvent(value) {
      let label = ""
      for (let i of this.events) {
        if (value === i.value) {
          label = i.label
          break
        }
      }
      return label
    },
    initData () {
      this.queryWebhooks()
    },
    queryWebhooks () {
      this.webhooks = []
      getWebhooks().then((res) => {
        this.webhooks = res
        if (this.webhooks) {
          this.webhooks.forEach(item => {
            let eventList = []
            if (item.events) {
              this.events.forEach(event => {
                if (item.events.includes(event.value)) {
                  eventList.push(event)
                }
              })
            }
            item.eventList = eventList
          })
        }
      })
    },
    showWebhookInfo(item) {
      this.webhookResetForm()
      if (item) {
        this.webhookForm  = Object.assign({}, item)
      }
      this.webhookVisible = true
    },
    webhookDrawerClose() {
      this.webhookResetForm()
      this.webhookVisible = false
    },
    webhookFormSubmit() {
      this.$refs.webhookForm.validate(valid => {
        if (valid) {
          delete this.webhookForm.eventList
          if(this.webhookForm.uuid) {
            updateWebhook(this.webhookForm).then(res => {
              this.successMsg("更新webhook成功")
              this.queryWebhooks()
              this.webhookDrawerClose()
            }).catch((err) => {
              let msg = err.response.data.message?err.response.data.message:err.response.data.error?err.response.data.error:err.response.data
              if (msg && msg.length > 0) {
                this.$notification.error({
                  message: msg,
                  description: ""
                })
              }
            })
          } else {
            saveWebhook(this.webhookForm).then(res => {
                this.successMsg("新增webhook成功")
                this.queryWebhooks()
                this.webhookDrawerClose()
              }).catch((err) => {
                let msg = err.response.data.message?err.response.data.message:err.response.data.error?err.response.data.error:err.response.data
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
    webhookResetForm() {
      if (this.$refs.webhookForm) {
        this.$refs.webhookForm.resetFields()
      }
    },
    deleteWebhook(item) {
      deleteWebhook({uuid: item.uuid}).then(res => {
        this.successMsg("删除webhook成功")
        this.queryWebhooks()
      }).catch((err) => {
      })
    },
    showWebhookLog(item) {
      this.webhookLogList = []
      queryWebhookLog({url: item.url}).then((res) => {
        this.webhookLogList = res
        this.webhookLogVisible = true
      })
    },
    webhookLogDrawerClose() {
      this.webhookLogVisible = false
    },
    webhookLogDelete(item) {
      deleteWebhookLog({id: item.id}).then((res) => {
        this.successMsg("删除webhook记录成功")
        this.showWebhookLog(item)
      })
    },
    showWebhookLogInfo(item) {
      this.webhookLogInfo  = Object.assign({}, item)
      if (this.webhookLogInfo.request) {
        this.webhookLogInfo.request = JSON.stringify(JSON.parse(this.webhookLogInfo.request), null, '\t')
      }
      if (this.webhookLogInfo.requestHeaders) {
        this.webhookLogInfo.requestHeaders = JSON.parse(this.webhookLogInfo.requestHeaders)
      } else {
        this.webhookLogInfo.requestHeaders = {}
      }
      if (this.webhookLogInfo.response) {
        this.webhookLogInfo.response = JSON.stringify(JSON.parse(this.webhookLogInfo.response), null, '\t')
      }
      if (this.webhookLogInfo.responseHeaders) {
        this.webhookLogInfo.responseHeaders = JSON.parse(this.webhookLogInfo.responseHeaders)
      } else {
        this.webhookLogInfo.responseHeaders = {}
      }
      this.webhookLogInfoVisible = true
    },
    webhookLogInfoDrawerClose() {
      this.webhookLogInfoVisible = false
    },
    doTestWebhook(event, item) {
      let e = this.events[event.key]
      testWebhook({uuid: item.uuid, events: [e.value]}).then((res) => {
        this.successMsg("测试[" + e.label + "]webhook成功")
      })
    }
  },
};
</script>

<style lang="scss" scoped>
  .webhook-test{
    color: black;
    font-size: 12px;
    font-weight: 600;
  }
</style>