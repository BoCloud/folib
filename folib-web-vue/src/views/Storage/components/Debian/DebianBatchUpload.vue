<template>
  <div>
    <a-modal v-model="visible" :title="$t('Store.Upload')" :maskClosable="false" centered :footer="null" @cancel="close">
      <a-form-model
        layout="horizontal"
        ref="uploadForm"
        :model="uploadForm"
        :rules="uploadRules"
        :hideRequiredMark="true"
        @submit.prevent="upload"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Store.TargetWarehouse')" :colon="false" prop="repositoryId">
              <a-input :disabled="true" :placeholder="$t('Store.InputTargetWarehouse')"
                v-model="uploadForm.repositoryId" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Store.Product')" :colon="false" prop="files">
              <a-upload v-model="uploadForm.files" :fileList="uploadForm.files" :multiple="true" :beforeUpload="beforeUpload" list-type="text"
                :accept="'.deb'">
                <a-button>
                  <a-icon type="upload" />
                  {{ $t('Store.SelectFile') }}</a-button>
              </a-upload>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item  class="mb-10" label="distribution" :colon="false" prop="distribution">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'distribution'" v-model="uploadForm.distribution" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item  class="mb-10" label="component" :colon="false"
              prop="component">
              <a-input :placeholder="$t('Store.PleaseEnter') + 'component'" v-model="uploadForm.component" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24" class="text-center">
            <a-button key="submit" class="px-30" size="small" type="primary" :loading="loading" @click="upload">
              {{ $t('Store.Upload') }}
            </a-button>
            <a-button key="back" @click="close" class="px-30 ml-10" size="small">{{ $t('Store.Cancel') }}</a-button>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>

<script>
import { uploadBatchArt } from '@/api/debian'
import { artifactCheck } from "@/utils/layoutUtil"

export default {
  props: {
    folibRepository: {
      type: Object,
      required: true
    },
  },

  data() {
    return {
      uploadRules: {
        repositoryId: [{ required: true, message: this.$t('Store.InputTargetWarehouse'), trigger: "blur" }],
        distribution: [{ required: true, message: this.$t('Store.PleaseEnter') + 'distribution', trigger: "blur" }],
        component: [{ required: true, message: this.$t('Store.PleaseEnter') + 'component', trigger: "blur" }],
        files: [{ required: true, validator: this.validateFiles, trigger: 'change' }]
      },
      uploadForm: {
        files: [],
        distribution: undefined,
        component: undefined,
        storageId: undefined,
        repositoryId: undefined,
      },
      visible: false,
      loading: false,
      uploading: false,
    };
  },
  methods: {
    openModal() {
      this.visible = true;
      this.uploadForm.repositoryId = this.folibRepository.id;
      this.uploadForm.storageId = this.folibRepository.storageId;
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
      const result = artifactCheck(this.folibRepository, file.name, file.size);
      if (!result.check) {
        this.message(0, "warning", result.msg);
        return false;
      }
      const isExist = this.uploadForm.files.some(item => item.name === file.name && item.size === file.size);
      if (isExist) {
        this.message(0, "warning", "This file has already been added.");
        return false;
      }
      this.uploadForm.files = [...this.uploadForm.files, file];
      return false;
    },
    close() {
      this.visible = false;
      this.loading = false;
      this.uploadForm = {
        files: [],
        distribution: undefined,
        component: undefined,
        storageId: undefined,
        repositoryId: undefined,
      };
    },
    upload() {
      this.$refs.uploadForm.validate(valid => {
        if (valid) {
          this.handlerUploadFile();
        }
      });
    },
    async handlerUploadFile() {
      this.loading = true;
      this.uploading = true;
      try {
        const formData = new FormData();
        formData.append("storageId", this.folibRepository.storageId);
        formData.append("repositoryId", this.folibRepository.id);
        this.uploadForm.files.forEach(file => formData.append("files", file));
        formData.append("distribution", this.uploadForm.distribution);
        formData.append("component", this.uploadForm.component);

        await uploadBatchArt(formData);
        this.message(0, "success", this.$t("Store.OperationSuccess"));
        this.close();
      } catch (err) {
        const msg = err.response?.data?.error || err.response?.data || err.message;
        console.error('Upload error:', msg);
        this.message(err.response?.status, "error", msg);
      } finally {
        this.loading = false;
        this.uploading = false;
      }
    },
    validateFiles(rule, value, callback) {
      if (!value || value.length === 0) {
        callback(new Error(this.$t('Store.PleaseSelectFile')));
      } else {
        callback();
      }
    },
  }
}
</script>