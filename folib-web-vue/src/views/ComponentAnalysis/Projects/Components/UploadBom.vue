<template>
    <a-modal
        title="上传Bom"
        :visible="visible"
        @ok="handleOk"
        @cancel="handleCancel"
    >
        <div>
            <a-upload
                name="file"
                :showUploadList="false"
                :before-upload="beforeUpload"
            >
                <div class="custom-file-label">
                    {{fileList[0] ? fileList[0].name : $t('Projects.UploadPlaceholder')}}
                </div>
            </a-upload>
        </div>
        <template slot="footer">
            <a-button key="cancel" @click="handleCancel">
                {{$t('Projects.Cancel')}}
            </a-button>
            <a-button key="reset" @click="handleReset">
                {{$t('Projects.Reset')}}
            </a-button>
            <a-button key="submit" type="primary" :disabled="!fileList.length" :loading="loading" @click="handleOk">
                {{$t('Projects.Upload')}}
            </a-button>
        </template>
    </a-modal>
</template>
<script>
import { uploadBom } from "@/api/projects";
export default {
    data() {
        return {
            visible: false,
            loading: false,
            fileList: [],
            uuid: ''
        }
    },
    methods: {
        handleOpen(uuid) {
            this.visible = true
            this.uuid = uuid
        },
        beforeUpload(file) {
            this.fileList = [];
            this.fileList.push(file);
            return false;
        },
        handleOk() {
            const param = new FormData()
            param.append("project", this.uuid)
            param.append("multiPart", this.fileList[0])
            this.loading = true
            uploadBom(param).then(res => {
                this.$message.success('上传成功')
                this.handleCancel()
            }).finally(() => {
                this.loading = false
            })
        },
        handleReset() {
            this.fileList = [];
        },
        handleCancel() {
            this.fileList = [];
            this.visible = false
        },
    }
}
</script>

<style scoped lang="scss">
    ::v-deep .ant-upload.ant-upload-select {
        display: block;
    }
    .custom-file-label {
        padding: 0.375rem 0.75rem;
        line-height: 1.5;
        color: #bfcfdf;
        border-radius: 0.3rem;
        background: rgba(37, 61, 85, 0.1);
        cursor: pointer;
    }
</style>