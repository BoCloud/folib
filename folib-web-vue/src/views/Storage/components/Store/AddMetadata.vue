<template>
  <div>
    <a-modal
      v-model="showMetadata"
      :title="handlerMetadataType === 1 ? '新增元数据' : '修改元数据'"
      :maskClosable="false"
      cancelText="取消"
      okText="确定"
      @cancel="metadataHandlerCancel()"
      @ok="metadataHandlerConfirm()"
      centered
    >
      <a-form-model
        layout="horizontal"
        ref="metadataForm"
        :model="metadataForm"
        :rules="metadataRules"
        :hideRequiredMark="true"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item
              class="mb-10"
              label="自定义KEY"
              :colon="false"
              prop="custom"
            >
              <a-switch
                :disabled="handlerMetadataType !== 1"
                v-model="metadataForm.custom"
                @change="metadataCustom"
              />
            </a-form-model-item>
          </a-col>
          <a-col :span="24" v-if="!metadataForm.custom">
            <a-form-model-item
              class="mb-10"
              label="元数据KEY"
              :colon="false"
              prop="key"
            >
              <a-select
                :disabled="handlerMetadataType !== 1"
                v-model="metadataForm.key"
                @change="metadataKeyChange"
                placeholder="请选择元数据KEY"
                show-search
                optionFilterProp="value"
              >
                <a-select-option
                  v-for="(item, index) in metadataConfigList"
                  :key="index"
                  :value="item.key"
                >
                  {{ item.key }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24" v-if="metadataForm.custom">
            <a-form-model-item
              class="mb-10"
              label="元数据KEY"
              :colon="false"
              prop="customKey"
            >
              <a-input
                :disabled="handlerMetadataType !== 1"
                placeholder="请输入元数据KEY"
                v-model="metadataForm.customKey"
              />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item
              class="mb-10"
              label="元数据类型"
              :colon="false"
              prop="type"
            >
              <a-select
                :disabled="!metadataForm.custom"
                v-model="metadataForm.type"
                @change="metadataTypeChange"
                placeholder="请选择元数据类型"
                show-search
                optionFilterProp="label"
              >
                <a-select-option
                  v-for="(item, index) in metadataTypes"
                  :label="item.label"
                  :key="index"
                  :value="item.value"
                >
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item
              class="mb-30"
              label="元数据值"
              :colon="false"
              prop="value"
            >
              <a-input
                v-if="metadataInput"
                placeholder="请输入元数据值"
                v-model="metadataForm.value"
              />
              <a-input-number
                v-if="metadataNumber"
                style="width: 100%"
                placeholder="请输入元数据值"
                v-model="metadataForm.value"
              />
              <quill-editor
                v-if="metadataEditor"
                v-model="metadataForm.value"
                :options="quillOptions"
                style="height: 300px"
              />
              <prism-editor
                class="metadata-prism-editor"
                v-if="prismEditor"
                v-model="metadataForm.value"
                :highlight="highlighterHandle"
                :line-numbers="true"
                placeholder="在此处输入内容"
                :readonly="false"
              >
              </prism-editor>
            </a-form-model-item>
          </a-col>
          <!-- <a-col :span="24" v-if="metadataForm.custom">
            <a-form-model-item class="mb-10" label="是否展示" :colon="false" prop="viewShow">
              <a-switch v-model="metadataForm.viewShow" />
            </a-form-model-item>
          </a-col> -->
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>
<script>
import { saveArtifactMetadata, updateArtifactMetadata } from "@/api/artifact";

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
export default {
  props: [
    "showMetadataHandler",
    "handlerMetadataType",
    "propMetadataForm",
    "metadataConfigList",
    "currentTreeNode",
    "metadataTypes",
    "successMsg",
    "quillOptions"
  ],
  components: {
    PrismEditor,
    quillEditor,
  },
  data() {
    return {
      metadataRules: {
        key: [{ required: true, message: "请选择元数据KEY", trigger: "blur" }],
        customKey: [
          { required: true, message: "请输入元数据KEY", trigger: "blur" },
          {
            min: 1,
            max: 30,
            message: "长度在 1 到 30 个字符",
            trigger: "blur",
          },
        ],
        type: [
          { required: true, message: "请选择元数据类型", trigger: "blur" },
        ],
        value: [{ required: true, message: "请输入元数据值", trigger: "blur" }],
      },
       metadataForm: {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: undefined,
        viewShow: true,
        value: undefined,
      },
      metadataEditor: false,
      prismEditor: false,
      metadataInput: false,
      metadataNumber: false,
      showMetadata: false,
    };
  },
  created() {
    if (this.propMetadataForm) {
      this.metadataForm = Object.assign({},this.propMetadataForm)
      this.metadataTypeChange(this.metadataForm.type);
    }
    if (this.showMetadataHandler) {
      this.showMetadata = this.showMetadataHandler
    }
  },
  mounted() {},
  methods: {
    metadataHandlerCancel() {
      this.$emit("metadataHandlerCancel");
    },
    metadataCustom(value) {
      if (value) {
        //开启自定义
        this.metadataForm.key = undefined;
        this.metadataForm.type = undefined;
      } else {
        //使用全局配置KEY
        this.metadataForm.customKey = undefined;
        this.metadataForm.type = undefined;
      }
    },
    // metadataCustom(value) {
    //   this.$emit("metadataCustom", value);
    // },
    metadataKeyChange(value) {
      let type = null;
      this.metadataConfigList.forEach((config) => {
        if (config.key === value) {
          type = config.type;
        }
      });
      this.metadataForm.type = type;
      this.metadataTypeChange(type);
    },
    // metadataKeyChange(value) {
    //   this.$emit("metadataKeyChange", value);
    // },
    metadataTypeChange(value) {
      let editorList = ["TEXT", "MD"];
      let prismEditorList = ["JSON"];
      let numberList = ["NUMERICAL"];
      if (editorList.indexOf(value) !== -1) {
        this.metadataEditor = true;
        this.metadataInput = false;
        this.metadataNumber = false;
        this.prismEditor = false;
      } else if (prismEditorList.indexOf(value) !== -1) {
        this.prismEditor = true;
        this.metadataInput = false;
        this.metadataNumber = false;
        this.metadataEditor = false;
      } else if (numberList.indexOf(value) !== -1) {
        if (this.handlerMetadataType === 1) {
          this.metadataForm.value = '';
        }
        this.metadataNumber = true;
        this.metadataInput = false;
        this.prismEditor = false;
        this.metadataEditor = false;
      } else {
        this.metadataInput = true;
        this.metadataEditor = false;
        this.metadataNumber = false;
        this.prismEditor = false;
      }
    },
    metadataHandlerConfirm() {
      this.$refs.metadataForm.validate((valid) => {
        if (valid) {
          let data = Object.assign({}, this.metadataForm);
          if (data.viewShow) {
            data.viewShow = 1;
          } else {
            data.viewShow = 0;
          }
          if (!data.custom) {
            this.metadataConfigList.forEach((config) => {
              if (config.key === data.key) {
                data.type = config.type;
                data.viewShow = config.viewShow;
              }
            });
          } else {
            data.key = data.customKey;
          }
          data.storageId = this.currentTreeNode.storageId;
          data.repositoryId = this.currentTreeNode.repositoryId;
          data.artifactPath = this.currentTreeNode.artifactPath;
          delete data.custom;
          delete data.customKey;
          if (this.handlerMetadataType === 1) {
            saveArtifactMetadata(data)
              .then((res) => {
                if (res === "repeat") {
                  this.$notification["warning"]({
                    message: "元数据KEY已存在",
                    description: "",
                  });
                  return false;
                }
                this.successMsg("新增制品元数据成功");
                this.$emit("metadataReflesh");
              })
              .finally(() => {});
          } else {
            updateArtifactMetadata(data)
              .then((res) => {
                this.successMsg("修改制品元数据成功");
                this.$emit("metadataReflesh");
              })
              .finally(() => {});
          }
        } else {
          return false;
        }
      });
    },
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
  },
};
</script>