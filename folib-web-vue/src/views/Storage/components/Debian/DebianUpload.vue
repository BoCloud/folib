<template>
  <div>
    <a-modal v-model="visible" :title="$t('Store.Upload')" :maskClosable="false" centered :footer="null" @cancel="close">
      <a-form-model layout="horizontal" ref="uploadForm" :model="uploadForm" :rules="uploadRules" :hideRequiredMark="true"
        @submit.prevent="upload">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Store.TargetWarehouse')" :colon="false" prop="repositoryId">
              <a-input :disabled="true" :placeholder="$t('Store.InputTargetWarehouse')"
                v-model="uploadForm.repositoryId" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Store.Product')" :colon="false" prop="file">
              <a-upload name="file" :multiple="false" :before-upload="beforeUpload" :remove="handleRemove"
                :fileList="fileList" :customRequest="customRequest" :accept="'.deb'">
                <a-button> <a-icon type="upload" />{{ $t('Store.SelectFile') }}</a-button>
                <a-progress :percent="percent" v-if="percent > 0" size="small"
                  :stroke-color="{ '0%': '#108ee9', '100%': '#108ee9', }" />
              </a-upload>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item  class="mb-10" label="distribution" :colon="false" prop="distribution">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'distribution'" v-model="uploadForm.distribution" required />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item  class="mb-10" label="component" :colon="false"
              prop="component">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'component'" v-model="uploadForm.component" required/>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item  class="mb-10" label="architecture" :colon="false" prop="architecture">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'architecture'" v-model="uploadForm.architecture" required />
            </a-form-model-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" :loading="loading" @click="upload">{{
              $t('Store.Upload') }}</a-button>
            <a-button key="back" @click="close" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>
<script>
import {parseArtifact,uploadArt  } from '@/api/debian'
import { artifactCheck} from "@/utils/layoutUtil"

export default {
  props: {
    folibRepository: {
    },
  },
  components: {

  },
  data() {
    return {
      uploadRules: {
        repositoryId: [{ required: true, message: this.$t('Store.InputTargetWarehouse'), trigger: "blur" }],
        distribution: [{ required: true, message: this.$t('Store.PleaseEnter') + 'distribution', trigger: "blur" }],
        component: [{ required: true, message: this.$t('Store.PleaseEnter') + 'component', trigger: "blur" }],
        architecture: [{ required: true, message: this.$t('Store.PleaseEnter') + 'architecture', trigger: "blur" }],
      },
      uploadForm:{
        storageId:undefined,
        repositoryId:undefined,
        version:undefined,
        fileName:undefined,
        architecture:undefined,
        path:undefined,
        distribution:undefined,
        component:undefined,
      },
      visible: false,
      debianMeta: {
        version: undefined,
        fileName: undefined,
        architecture: undefined,
        path: undefined,
      },
      loading: false,
      percent: 0,
      fileList: [],
    };
  },
  methods: {

    openModal() {
      this.visible = true;
      this.uploadForm.repositoryId = this.folibRepository.id
      this.uploadForm.storageId = this.folibRepository.storageId
    },

    message(status, type, message) {
      let statusList = [401, 403]
      if (statusList.includes(status)) {
        return
      }
      if (!message) {
        message = this.$t("Store.OperationSuccess")
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
    beforeUpload(file) {
      if (this.uploadForm.file) {
        this.message(0, "warning", this.$t("Store.ArtifactSingleUploadLimit"))
        return false
      }
      let result = artifactCheck(this.folibRepository, file.name, file.size)
      if (!result.check) {
        this.message(0, "warning", result.msg)
        return false
      }
      this.uploadForm.file = file
      this.fileList.push(file)
    },
    handleRemove(file) {
      this.uploadForm.file = undefined;
      const index = this.fileList.indexOf(file)
      const newFileList = this.fileList.slice()
      newFileList.splice(index, 1)
      this.fileList = newFileList
    },
    normFile(e) {
      if (Array.isArray(e)) {
        return e
      }
      return e && e.fileList
    },
    close() {
      this.visible = false;
      this.loading = false;
      this.uploadForm={};
      this.fileList=[];
    },
    customRequest(info) {
      const uploadApi = ({ file, onUploadProgress }) => {
        const formData = new FormData();
        formData.append("storageId", this.folibRepository.storageId);
        formData.append("repositoryId", this.folibRepository.id);
        formData.append("file", file);
        return parseArtifact(formData,onUploadProgress);
      }
      uploadApi({
        file: info.file,
        onUploadProgress: (ev) => {
          this.percent = new Number(((ev.loaded / ev.total) * 100).toFixed(2))
        },
      }).then((res) => {
        this.percent = 0;
        this.debianMeta = res;
        this.uploadForm.architecture = this.debianMeta.architecture;
        this.uploadForm.path = this.debianMeta.path;
        this.uploadForm.version=this.debianMeta.version;
        this.uploadForm.fileName=this.debianMeta.fileName;
      }).catch((err) => {
        this.uploadForm={};
        this.fileList=[];
        let msg = err.response.data.error ? err.response.data.error : err.response.data
        this.message(err.response.status, "error", msg)
      })
    },
    upload() {
      this.$refs.uploadForm.validate((valid) => {
        if (valid) {
          this.handlerUploadFile()
        }
      })
    },
    handlerUploadFile() {
      this.loading = true
      uploadArt(this.uploadForm).then((res) => {
        this.loading = false
        this.close()
        this.message(0, "success", this.$t("Store.OperationSuccess"))
      }).catch((err) => {
        this.loading = false
        let msg = err.response.data.error ? err.response.data.error : err.response.data
        console.log('upload error：', msg)
        this.message(err.response.status, "error", msg)
      }).finally(() => {
        this.loading = false
      })
    },
  }
}
</script>