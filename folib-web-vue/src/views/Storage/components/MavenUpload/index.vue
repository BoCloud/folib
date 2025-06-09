<template>
  <div>
    <a-modal v-model="visible" :title="$t('Store.Upload')" :maskClosable="false" centered :footer="null">
      <a-form-model layout="horizontal" ref="uploadForm" :model="uploadForm" :rules="uploadRules" :hideRequiredMark="true"
        @submit.prevent="upload">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Store.TargetWarehouse')" :colon="false" prop="repositoryId">
              <a-input :disabled="true" :placeholder="$t('Store.InputTargetWarehouse')" v-model="uploadForm.repositoryId" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Store.Product')" :colon="false" prop="file">
              <a-upload name="file" :multiple="false" :before-upload="beforeUpload" :remove="handleRemove"
                :fileList="fileList" :customRequest="customRequest" :accept="'.jar,.war,.pom,.aar'">
                <a-button> <a-icon type="upload" />{{ $t('Store.SelectFile') }}</a-button>
                <a-progress :percent="percent" v-if="percent > 0" size="small"
                  :stroke-color="{ '0%': '#108ee9', '100%': '#108ee9', }" />
              </a-upload>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item v-if="this.parseArtifact.type" class="mb-10" label="GroupId" :colon="false" prop="groupId">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'GroupId'" v-model="uploadForm.groupId" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item v-if="this.parseArtifact.type" class="mb-10" label="ArtifactId" :colon="false"
              prop="artifactId">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'ArtifactId'" v-model="uploadForm.artifactId" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item v-if="this.parseArtifact.type" class="mb-10" label="Version" :colon="false" prop="version">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'Version'" v-model="uploadForm.version" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" :loading="loading" @click="upload">{{ $t('Store.Upload') }}</a-button>
            <a-button key="back" @click="close" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>
<script>
import { axios } from '@/utils/request'
import {
  singleArtifactUpload,
  parseArtifact,
} from "@/api/artifact"
import {
  artifactCheck,
} from "@/utils/layoutUtil"

export default {
  inject: ["reload"],
  props: {
    folibRepository: {
      type: Object,
      default: {},
    },
    modelVisible: {
      type: Boolean,
      default: false,
    },
  },
  components: {

  },
  data() {
    return {
      uploadRules: {
        repositoryId: [{ required: true, message: this.$t('Store.InputTargetWarehouse'), trigger: "blur" }],
        file: [{ required: true, message: this.$t('Store.SelectFile'), trigger: ["blur", "change"] }],
        groupId: [{ required: true, message: this.$t('Store.PleaseEnter') + 'GroupId', trigger: "blur" }],
        artifactId: [{ required: true, message: this.$t('Store.PleaseEnter') + 'ArtifactId', trigger: "blur" }],
        version: [{ required: true, message: this.$t('Store.PleaseEnter') + 'Version', trigger: "blur" }],
      },
      uploadForm: {
        storageId: undefined,
        repositoryId: undefined,
        filePathMap: undefined,
        file: undefined,
        groupId: undefined,
        artifactId: undefined,
        version: undefined,
        filePath: undefined,
      },
      visible: false,
      parseArtifact: {
        type: undefined,
        groupId: undefined,
        artifactId: undefined,
        version: undefined,
        filePath: undefined,
      },
      loading: false,
      percent: 0,
      fileList: [],
    };
  },
  created() {
    if (this.modelVisible) {
      this.visible = this.modelVisible
      this.uploadForm.repositoryId = this.folibRepository.id
      this.uploadForm.storageId = this.folibRepository.storageId
    }
  },
  mounted() { },
  methods: {
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
      this.uploadForm.file = undefined
      this.uploadForm.groupId = undefined
      this.uploadForm.artifactId = undefined
      this.uploadForm.version = undefined
      this.uploadForm.filePath = undefined
      const index = this.fileList.indexOf(file)
      const newFileList = this.fileList.slice()
      newFileList.splice(index, 1)
      this.fileList = newFileList
      this.parseArtifact = {
        type: undefined,
        groupId: undefined,
        artifactId: undefined,
        version: undefined,
        filePath: undefined,
      }
    },
    normFile(e) {
      if (Array.isArray(e)) {
        return e
      }
      return e && e.fileList
    },
    close() {
      this.$emit('mavenUploadClose')
    },
    customRequest(info) {
      const uploadApi = ({ file, onUploadProgress }) => {
        const formData = new FormData()
        formData.append("storageId", this.folibRepository.storageId)
        formData.append("repositoryId", this.folibRepository.id)
        formData.append("file", file)
        return axios({
          url: '/api/artifact/folib/promotion/parseArtifact',
          method: 'post',
          timeout: 15 * 60 * 1000,
          headers: { "Content-type": "multipart/form-data", },
          data: formData,
          onUploadProgress,
        });
      }
      uploadApi({
        file: info.file,
        onUploadProgress: (ev) => {
          this.percent = new Number(((ev.loaded / ev.total) * 100).toFixed(2))
        },
      }).then((res) => {
        this.percent = 0
        this.parseArtifact = res
        this.uploadForm.groupId = this.parseArtifact.groupId
        this.uploadForm.artifactId = this.parseArtifact.artifactId
        this.uploadForm.version = this.parseArtifact.version
        this.uploadForm.filePath = this.parseArtifact.filePath
        if (res.type === 2) {
          this.message(0, "warning", this.$t("Store.NonStandardArtifact"))
        }
      }).catch((err) => {
        let msg = err.response.data.error ? err.response.data.error : err.response.data
        console.log('upload error：', msg)
        let errStatusArr = [200, 500, 403, 304, 401]
        if (!errStatusArr.includes(err.response.status)) {
          this.message(err.response.status, "error", "err code:" + err.response.status)
        }
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
      if (!this.parseArtifact.type) {
        return false
      }
      let formData = new FormData()
      formData.append("storageId", this.folibRepository.storageId)
      formData.append("repositoryId", this.folibRepository.id)
      let artifact = {
        groupId: this.uploadForm.groupId,
        artifactId: this.uploadForm.artifactId,
        version: this.uploadForm.version,
        filePath: this.uploadForm.filePath,
      }
      formData.append("parseArtifact", JSON.stringify(artifact))
      this.loading = true
      singleArtifactUpload(formData).then((res) => {
        // this.loading = false
        this.close()
        this.reload()
        this.message(0, "success", this.$t("Store.OperationSuccess"))
      }).catch((err) => {
        // this.loading = true
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