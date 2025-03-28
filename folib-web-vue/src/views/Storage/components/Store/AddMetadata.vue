<template>
  <div>
    <a-modal
      v-model="showMetadata"
      :title="handlerMetadataType === 1 ? $t('Store.Create')+$t('Store.Metadata') : $t('Store.Modify')+$t('Store.Metadata')"
      :maskClosable="false"
      :cancelText="$t('Store.Cancel')"
      :okText="$t('Store.Confirm')"
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
              :label="$t('Store.Custom')+'KEY'"
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
              :key='key'
              class="mb-10"
              :label="$t('Store.Metadata')+'KEY'"
              :colon="false"
              prop="key"
            >
              <a-select
                :disabled="handlerMetadataType !== 1"
                v-model="metadataForm.key"
                @change="metadataKeyChange"
                :placeholder="$t('Store.PleaseSelect')+$t('Store.Metadata')+'KEY'"
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
          <a-col :span="24" v-else>
            <a-form-model-item
              :key='key'
              class="mb-10"
              :label="$t('Store.Metadata')+'KEY'"
              :colon="false"
              prop="customKey"
            >
              <a-input
                :disabled="handlerMetadataType !== 1"
                :placeholder="$t('Store.PleaseEnter')+$t('Store.Metadata')+'KEY'"
                v-model="metadataForm.customKey"
              />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item
              class="mb-10"
              :label="$t('Store.Metadata')+$t('Store.Type')"
              :colon="false"
              prop="type"
            >
              <a-select
                :disabled="!metadataForm.custom"
                v-model="metadataForm.type"
                @change="metadataTypeChange"
                :placeholder="$t('Store.PleaseSelect')+$t('Store.Metadata')+$t('Store.Type')"
                show-search
                optionFilterProp="label"
              >
                <a-select-option
                  v-for="(item, index) in i18nMetadataTypes"
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
              :label="$t('Store.Metadata')+$t('Store.value')"
              :colon="false"
              prop="value"
            >
              <a-input
                v-if="metadataInput"
                :placeholder="$t('Store.PleaseEnter')+$t('Store.Metadata')+$t('Store.value')"
                v-model="metadataForm.value"
              />
              <a-input-number
                v-if="metadataNumber"
                style="width: 100%"
                :placeholder="$t('Store.PleaseEnter')+$t('Store.Metadata')+$t('Store.value')"
                v-model="metadataForm.value"
              />
              <quill-editor
                v-if="metadataEditor"
                v-model="metadataForm.value"
                :options="quillOptions"
                style="height: 300px; margin-bottom: 20px"
              />
              <prism-editor
                class="metadata-prism-editor"
                v-if="prismEditor"
                v-model="metadataForm.value"
                :highlight="highlighterHandle"
                :line-numbers="true"
                :placeholder="$t('Store.EnterSomethingHere')"
                :readonly="false"
              >
              </prism-editor>
            </a-form-model-item>
          </a-col>
          <a-col :span="24" v-if="currentTreeNode.type == 'dir'">
            <a-form-model-item class="mb-10" :colon="false" prop="recursive">
              <template slot="label">
                {{ $t('Store.Recursive') }}
                <a-popover placement="topLeft">
                  <template slot="content">
                    <p class="mb-0">{{ $t('Store.RecursiveTip') }}</p>
                  </template>
                  <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                </a-popover>
              </template>
              <a-switch v-model="metadataForm.recursive" />
            </a-form-model-item>
          </a-col>
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
        key: [{ required: true, message: this.$t('Store.PleaseSelect')+this.$t('Store.Metadata')+'KEY',trigger: "blur" }],
        customKey: [
          { required: true, message: this.$t('Store.PleaseEnter')+this.$t('Store.Metadata')+'KEY',trigger: "blur"},
          {
            min: 1,
            max: 30,
            message: this.$t('Store.KEYLength'),
            trigger: "blur",
          },
        ],
        type: [
          { required: true, message: this.$t('Store.PleaseSelect')+this.$t('Store.Metadata')+this.$t('Store.Type'), trigger: "blur" },
        ],
        value: [{ required: true, message: this.$t('Store.PleaseEnter')+this.$t('Store.Metadata')+this.$t('Store.value'), trigger: "blur" }],
      },
       metadataForm: {
        key: undefined,
        customKey: undefined,
        custom: false,
        type: undefined,
        viewShow: true,
        value: undefined,
        recursive: false,
      },
      metadataEditor: false,
      prismEditor: false,
      metadataInput: false,
      metadataNumber: false,
      showMetadata: false,
      key:0
    };
  },
  computed: {
    i18nMetadataTypes() {
      return this.metadataTypes.map(item => {
        if (item.i18nKey) {
          item.label = this.$t(item.i18nKey);
        }
        return item;
      });
    }
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
      this.key ++
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
                    message: this.$t('Store.Metadata')+'KEY'+this.$t('Store.ItExists'),
                    description: "",
                  });
                  return false;
                }
                this.successMsg(this.$t('Store.Create')+this.$t('Store.MetadataSuccess'));
                this.$emit("metadataReflesh");
              }).catch((err) => {
                let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
                if (msg && msg.length > 0) {
                  this.$notification.error({
                    message: msg,
                    description: ""
                  })
                }
              }).finally(() => {});
          } else {
            updateArtifactMetadata(data).then((res) => {
              this.successMsg(this.$t('Store.Modify')+this.$t('Store.MetadataSuccess'));
              this.$emit("metadataReflesh");
            }).catch((err) => {
              let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
              if (msg && msg.length > 0) {
                this.$notification.error({
                  message: msg,
                  description: ""
                })
              }
            }).finally(() => {});
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
