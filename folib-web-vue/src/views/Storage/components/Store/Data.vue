<template>
  <div>
    <a-tabs default-active-key="1" @change="artifactTabChange">
      <a-tab-pane key="1" tab="基本信息">
        <a-descriptions
          v-if="folibRepository.layout !== 'Docker'"
          title=""
          :column="1"
          style="word-break: break-all;word-wrap: break-word;"
        >
          <a-descriptions-item label="所属空间">
            {{ currentTreeNode.storageId }}
          </a-descriptions-item>
          <a-descriptions-item label="所属仓库">
            {{ currentTreeNode.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item label="名称">
            {{ currentTreeNode.name }}
          </a-descriptions-item>
          <a-descriptions-item label="路径">
            {{ currentTreeNode.artifactPath }}
          </a-descriptions-item>
          <a-descriptions-item label="文件大小">
            {{ fileSizeConver(currentTreeNode.size) }}
          </a-descriptions-item>
          <a-descriptions-item label="修改时间">
            {{ formateDate(currentTreeNode.lastModified) }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="最近使用时间">
            {{ currentFileDetial.lastUsedTime }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="下载次数">
            {{ currentFileDetial.downloadCount }}
          </a-descriptions-item>
          <template v-if="currentFileDetial && currentFileDetial.artifact && currentFileDetial.artifact.checksums" >
            <a-descriptions-item :label="key" v-for="(value, key, index) in currentFileDetial.artifact.checksums" :key="index" span="2">
              {{ value }}
            </a-descriptions-item>
          </template>
        </a-descriptions>
        <a-descriptions
          v-if="folibRepository.layout === 'Docker'"
          title=""
          :column="1"
        >
          <a-descriptions-item label="所属空间">
            {{ currentTreeNode.storageId }}
          </a-descriptions-item>
          <a-descriptions-item label="所属仓库">
            {{ currentTreeNode.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="镜像名称">
            {{ currentFileDetial.imageName }}
          </a-descriptions-item>
          <a-descriptions-item :label="currentFileDetial ? '版本号' : '名称'">
            {{ currentTreeNode.name }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="文件大小">
            {{ fileSizeConver(currentFileDetial.size) }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="SHA-256">
            {{ currentFileDetial.sha256 }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="修改时间">
            {{ currentFileDetial.lastModified }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="层数">
            {{ currentFileDetial.manifest.layers.length }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="制作Docker版本">
            {{ currentFileDetial.manifestConfig.docker_version }}
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="镜像OS">
            <a-tag> {{ currentFileDetial.manifestConfig.os }}</a-tag>
          </a-descriptions-item>
          <a-descriptions-item v-if="currentFileDetial" label="基础架构">
            {{ currentFileDetial.manifestConfig.architecture }}
          </a-descriptions-item>
        </a-descriptions>
      </a-tab-pane>
      <a-tab-pane key="2" tab="元数据">
        <a v-if="metadataEnabled" @click="metadataHandler()">
          <a-tooltip>
            <template slot="title">新增</template>
            <a-icon type="plus-circle" theme="filled" class="ml-30"
              :style="{ fontSize: '28px', color: '#1890FF' }" />
          </a-tooltip>
        </a>
        <a-table :columns="metadataColumns" :data-source="metadataList" rowKey="key">
          <div slot="type" slot-scope="type">
            <span v-for="(item, index) in metadataTypes" :key="index">
              <span v-if="type === item.value">
                <a-tag color="#87d068"> {{ item.label }} </a-tag>
              </span>
            </span>
          </div>
          <div slot="value" slot-scope="text, record">
            <span v-if="record.type === 'NUMERICAL'">
              {{ fixedNumber(record.value) }}
            </span>
            <span
              v-if="
                record.type !== 'TEXT' &&
                record.type !== 'MD' &&
                record.type !== 'JSON' &&
                record.type !== 'NUMERICAL'
              "
            >
              {{ record.value }}
            </span>
            <a-button
              type="link"
              size="small"
              v-if="record.type === 'TEXT' || record.type === 'MD'"
              @click="metadataEditorDrawerShow(record)"
            >
              查看
            </a-button>
            <a-button
              type="link"
              size="small"
              v-if="record.type === 'JSON'"
              @click="metadataPrismEditorDrawerShow(record)"
            >
              查看
            </a-button>
          </div>
          <div slot="operation" slot-scope="text, record">
            <div class="col-action" v-if="$store.state.user.token">
              <a-popconfirm
                title="确定要删除吗？"
                okType="danger"
                ok-text="确定"
                cancel-text="取消"
                @confirm="deleteArtifactMetadata(record.key)"
              >
                <a-button type="link" size="small">
                  <svg
                    width="16"
                    height="16"
                    viewBox="0 0 20 20"
                    fill="none"
                    xmlns="http://www.w3.org/2000/svg"
                  >
                    <path
                      class="fill-danger"
                      fill-rule="evenodd"
                      clip-rule="evenodd"
                      d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                      fill="#111827"
                    />
                  </svg>
                  <span class="text-danger">DELETE</span>
                </a-button>
              </a-popconfirm>
              <a-button
                type="link"
                size="small"
                @click="metadataEditHandler(record)"
              >
                <svg
                  width="16"
                  height="16"
                  viewBox="0 0 20 20"
                  fill="none"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <path
                    class="fill-muted"
                    d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                    fill="#111827"
                  />
                  <path
                    class="fill-muted"
                    d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                    fill="#111827"
                  />
                </svg>
                <span class="text-dark">EDIT</span>
              </a-button>
            </div>
          </div>
        </a-table>
      </a-tab-pane>
    </a-tabs>
    <hr class="my-25" />

    <a-col
      :span="24"
      v-if="
        currentFileDetial &&
        currentFileDetial.snippets &&
        currentFileDetial.snippets.length > 0
      "
    >
      <a-card :bordered="false" class="card-billing-info">
        <div class="col-info">
          <a-descriptions
            :title="'使用示例(' + codeParam.type + ')'"
            :column="1"
          >
            <a-descriptions-item v-if="currentFileDetial">
              <prism-editor
                class="my-editor height-300"
                v-if="currentFileDetial"
                v-model="codeParam.code"
                :highlight="highlighterHandle"
                :line-numbers="false"
                :readonly="true"
              ></prism-editor>
            </a-descriptions-item>
          </a-descriptions>
        </div>
        <div class="col-action">
          <a-button
            v-for="(item, index) in this.currentFileDetial.snippets"
            :key="index"
            type="link"
            size="small"
            @click="changeCodeTye(item)"
          >
            <a-avatar
              :size="20"
              shape="square"
              :src="'images/folib/' + getCodeImg(item) + '.svg'"
            />
          </a-button>
        </div>
      </a-card>
    </a-col>

    <!-- 编辑 -->
    <a-drawer
      placement="right"
      width="40%"
      :title="metadataPrismEditorDrawerTitle"
      :visible="metadataPrismEditorDrawerVisible"
      @close="metadataPrismEditorDrawerClose()"
    >
      <prism-editor
        class="metadata-prism-editor"
        style="height: 90vh"
        v-model="metadataPrismEditorDrawerValue"
        :highlight="highlighterHandle"
        :line-numbers="true"
        :readonly="true"
      >
      </prism-editor>
    </a-drawer>

    <!-- 查看 -->
    <a-drawer
      placement="right"
      width="40%"
      :title="metadataEditorDrawerTitle"
      :visible="metadataEditorDrawerVisible"
      @close="metadataEditorDrawerClose()"
    >
      <quill-editor
        class=""
        :disabled="true"
        v-model="metadataEditorDrawerValue"
        :options="quillOptions"
        style="height: 85vh"
      />
    </a-drawer>
  </div>
</template>
<script>
import store from "store";
import { fileSizeConver, formateDate } from "@/utils/layoutUtil";
import { getArtifact } from "@/api/folib";
import {  deleteArtifactMetadata } from "@/api/artifact";
import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css"; // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/themes/prism-tomorrow.css";
import "quill/dist/quill.core.css";
import "quill/dist/quill.snow.css";
import { quillEditor } from "vue-quill-editor";
import { hasRole, isAdmin, isAnonymous, isLogin } from "@/utils/permission";

export default {
  name: "BaseData",
  props: [
    "currentTreeNode",
    "repositoryType",
    "currentFileDetial",
    "folibRepository",
    "successMsg",
  ],
  components: {
    PrismEditor,
    quillEditor,
  },
  data() {
    return {
      metadataList: [],
      metadataConfigList: [],
      metadataTypes: [
        {
          label: "数字",
          value: "NUMERICAL",
        },
        {
          label: "字符串",
          value: "STRING",
        },
        {
          label: "文本",
          value: "TEXT",
        },
        {
          label: "Markdown",
          value: "MD",
        },
        {
          label: "JSON",
          value: "JSON",
        },
      ],
      metadataColumns: [
        {
          title: "元数据KEY",
          dataIndex: "key",
          key: "key",
          width: 150,
        },
        {
          title: "元数据类型",
          dataIndex: "type",
          key: "type",
          width: 150,
          scopedSlots: { customRender: "type" },
        },
        {
          title: "元数据值",
          dataIndex: "value",
          key: "value",
          width: 300,
          scopedSlots: { customRender: "value" },
        },
        {
          title: "操作",
          dataIndex: "operation",
          width: 250,
          scopedSlots: { customRender: "operation" },
        },
      ],
      metadataEditorDrawerTitle: undefined,
      metadataEditorDrawerVisible: false,
      metadataEditorDrawerValue: undefined,
      metadataPrismEditorDrawerTitle: undefined,
      metadataPrismEditorDrawerValue: false,
      metadataPrismEditorDrawerVisible: undefined,
      metadataInput: true,
      metadataNumber: false,
      metadataEditor: false,
      prismEditor: false,
      metadataForm: {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: undefined,
        viewShow: true,
        value: undefined,
      },
      showMetadataHandler: false,
      handlerMetadataType: 1,
      codeParam: {
        type: "",
        code: null,
      },
      quillOptions: {
        modules: {
          toolbar: [
            [{ header: [1, 2, 3, false] }],
            ["bold", "italic", "underline"],
            [{ list: "ordered" }, { list: "bullet" }, "link"],
            ["clean"],
          ],
        },
      },
      metadataEnabled: false,
    };
  },
  created() {
    if (isLogin()){
      this.metadataShow()
    }
  },
  mounted() {
  },
  watch: {
    currentFileDetial: function (val) {
      if (val && val.snippets) {
        this.changeCodeTye(val.snippets[0])
      }
      this.metadataShow()
    },
    'currentTreeNode.artifactPath': function (newval, oldVal) {
      this.metadataList = []
      if (this.currentTreeNode.type === 'file') {
        this.getMetadata()
      }
    },
  },
  methods: {
    metadataShow() {
      this.metadataEnabled = isLogin() && this.folibRepository.type !== 'group' &&
                          this.currentFileDetial &&
                          this.currentFileDetial.artifact &&
                          this.currentFileDetial.artifact.artifactFileExists   
      if (!(typeof(this.metadataEnabled) == 'boolean')) {
        this.metadataEnabled = false
      }
    },
    metadataHandler() {
      this.$emit("metadataHandler", 1)
    },
    artifactTabChange(activeKey) {
      this.metadataShow()
      if (activeKey === "2") {
        this.getMetadata()
      }
    },
    getMetadata() {
      getArtifact(
        this.repositoryType,
        this.currentTreeNode.storageId,
        this.currentTreeNode.repositoryId,
        this.currentTreeNode.artifactPath
      ).then((res) => {
        this.handlerRespMetadata(res);
        this.$forceUpdate();
      });
    },
    handlerRespMetadata(res) {
      let metadataList = [];
      if (
        res.artifact &&
        res.artifact.metadata &&
        res.artifact.metadata.length > 0
      ) {
        let metadataJson = JSON.parse(res.artifact.metadata);
        for (let key in metadataJson) {
          let flag = this.metadataConfigList.some(
            (metadataConfig) =>
              !metadataConfig.viewShow && metadataConfig.key === key
          );
          if (flag) {
            metadataJson[key].viewShow = false;
          }
          let metadata = Object.assign({}, metadataJson[key]);
          metadata.key = key;
          metadataList.push(metadata);
        }
      }
      this.metadataList = metadataList;
    },
    metadataEditorDrawerShow(metadata) {
      this.metadataEditorDrawerTitle = metadata.key;
      this.metadataEditorDrawerValue = metadata.value;
      this.metadataEditorDrawerVisible = true;
    },
    metadataEditorDrawerClose() {
      this.metadataEditorDrawerVisible = false;
    },
    metadataPrismEditorDrawerShow(metadata) {
      this.metadataPrismEditorDrawerTitle = metadata.key;
      this.metadataPrismEditorDrawerValue = metadata.value;
      this.metadataPrismEditorDrawerVisible = true;
    },
    metadataPrismEditorDrawerClose() {
      this.metadataPrismEditorDrawerVisible = false;
    },
    deleteArtifactMetadata(metadataKey) {
      let data = {
        key: metadataKey,
        storageId: this.currentTreeNode.storageId,
        repositoryId: this.currentTreeNode.repositoryId,
        artifactPath: this.currentTreeNode.artifactPath,
      };
      deleteArtifactMetadata(data)
        .then((res) => {
          this.successMsg("删除制品元数据成功");
          this.getMetadata();
        })
        .finally(() => {});
    },
    metadataEditHandler(metadata) {
      this.$emit("metadataEditHandler", metadata);
    },
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
    changeCodeTye(item) {
      if (item) {
        this.codeParam = {
          type: item.name === "Maven 2" ? "maven" : item.name.toLowerCase(),
          code: item.code,
        };
      }
    },
    getCodeImg(item) {
      return item.name === "Maven 2" ? "maven_black" : item.name.toLowerCase();
    },
    fileSizeConver(size) {
      if (size) {
        return fileSizeConver(size);
      }
    },
    formateDate(time) {
      if (time) {
        var date = new Date(time);
        var Y = date.getFullYear() + "-";
        var M =
          (date.getMonth() + 1 < 10
            ? "0" + (date.getMonth() + 1)
            : date.getMonth() + 1) + "-";
        var D =
          (date.getDate() < 10 ? "0" + date.getDate() : date.getDate()) + " ";
        var h =
          (date.getHours() < 10 ? "0" + date.getHours() : date.getHours()) +
          ":";
        var m =
          (date.getMinutes() < 10
            ? "0" + date.getMinutes()
            : date.getMinutes()) + ":";
        var s =
          (date.getSeconds() < 10
            ? "0" + date.getSeconds()
            : date.getSeconds()) + "";
        return Y + M + D + h + m + s;
      }
    },
    fixedNumber(val) {
      if (val) {
        let newVal = new Number(val);
        return newVal;
      }
      return 0;
    },
  },
};
</script>
