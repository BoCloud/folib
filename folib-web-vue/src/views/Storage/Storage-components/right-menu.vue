/**
功能：右侧菜单
作者：张佳宁
日期：2024年11月6日14:12:50
**/
<template>
    <div class="context-menu" v-if="showContextMenu">
        <a-menu @click="handleRightClick">
            <a-menu-item v-if="currentTreeNode && currentTreeNode.type === 'dir' || currentTreeNode.treeType === 'root'" key="1">
                <a-icon type="reload"/>
                {{ $t('Store.Refresh') }}
            </a-menu-item>
            <template v-if="!currentTreeNode.treeType">
                <a-menu-item key="2" v-if="copyEnabled && !isTrashView">
                    <a-icon type="copy"/>
                    {{ $t('Store.Copy') }}
                </a-menu-item>
                <a-menu-item key="3" v-if="moveEnabled && !isTrashView">
                    <a-icon type="swap"/>
                    {{ $t('Store.Move') }}
                </a-menu-item>
                <a-menu-item key="4" v-if="deleteEnabled && currentTreeNode.name !== '.trash'">
                    <!-- <a-popconfirm :title="$t('Store.SuerDelete')" okType="danger"
                        :ok-text="$t('Store.Confirm')" :cancel-text="$t('Store.Cancel')" @confirm.stop="deletePackageHandle"  :style="{ zIndex: 2000 }"> -->
                    <a-icon type="delete"/>
                    {{ $t('Store.Delete') }}
                    <!-- </a-popconfirm> -->
                </a-menu-item>
                <a-menu-item key="5" v-if="dispatchEnabled && !isTrashView">
                    <a-icon type="retweet"/>
                    {{ $t('Store.Distribute') }}
                </a-menu-item>

                <a-menu-item
                    key="6"
                    v-if="folibRepository.layout !== 'Docker' && currentTreeNode && currentTreeNode.type === 'file'
                     && currentFileDetial && currentFileDetial.artifact && !isTrashView"
                >
                    <a-icon type="download"/>
                    {{ $t('Store.DownLoad') }}
                </a-menu-item>
                <a-menu-item
                    key="7"
                    v-if="
                    folibRepository.layout === 'Docker' && currentTreeNode && currentTreeNode.type === 'file'
                    && currentFileDetial && currentFileDetial.artifact && !isTrashView
                    ||
                    folibRepository.layout === 'Raw' && currentTreeNode && currentTreeNode.type === 'dir'&&!isTrashView
                    "
                >
                    <a-icon type="download"/>
                    {{ $t('Store.DownLoad') }}
                </a-menu-item>
                <a-menu-item key="8" v-if="isTrashView && currentTreeNode">
                    <a-icon type="undo"/>
                    {{ $t('Store.Restore') }}
                </a-menu-item>
            </template>

        </a-menu>
    </div>
</template>

<script>
import { deleteArtifact, getArtifactPermission } from '@/api/folib'
import { hasRole, isAdmin, isAnonymous, isLogin } from '@/utils/permission'

export default {
    props: [
        'currentTreeNode',
        'folibRepository',
        'repositoryType',
        'currentFileDetial',
        'isTrashView',
        'uploadEnabled',
        'dispatchEnabled',
        'moveEnabled',
        'showContextMenu',
        'copyEnabled',
        'rightClickTop',
        'rightClickLeft'
    ],
    data() {
        return {
            deleteEnabled: false,
            type: ''
        }
    },
    inject: ['reload'],
    mounted() {

    },
    methods: {
        handleRightClick(active) {
            this.handleMenuClick(active)
            if (active.key === '4') {
                this.deletePackageHandle()
            }
        },
        // 右键菜单选择操作
        handleMenuClick(active) {
            this.$emit('handleMenuClick', active)
        },
        deletePackageHandle() {
            const {storageId, repositoryId, artifactPath} = this.currentTreeNode
            deleteArtifact(
                storageId,
                repositoryId,
                artifactPath
            )
                .then(res => {
                    setTimeout(() => {
                        this.$notification.success({
                            message: this.$t('Store.DeletionSuccessful')
                        })
                        // this.$emit('reload')
                        this.$emit('localDelNode', {storageId, repositoryId, artifactPath, type: this.type}, this.isTrashView)
                    }, 100)
                })
                .catch(err => {
                    let errStatusArr = [403, 401]
                    if (errStatusArr.includes(err.response.status)) {
                        return false
                    }
                    let msg = err.response.data.message
                        ? err.response.data.message
                        : err.response.data.error
                            ? err.response.data.error
                            : err.response.data
                    let temp = this.$t('Store.' + msg)
                    if (temp.indexOf('Store.') == -1) {
                        msg = temp 
                    }
                    if (!msg || msg.length === 0 || typeof msg === 'object') {
                        msg = this.$t('Store.DeletionFailed')
                    }
                    this.$notification.error({
                        message: msg,
                        description: ''
                    })
                })
                .finally(() => {
                })
        },
        handlerDataPermission(currentTreeNode, type, callback) {
            this.type = type
            this.deleteEnabled = false
            if (!currentTreeNode.storageId || !currentTreeNode.repositoryId || !currentTreeNode.artifactPath) {
                if (callback) callback()
                return false
            }
            let permissions = []
            getArtifactPermission(
                currentTreeNode.storageId,
                currentTreeNode.repositoryId,
                currentTreeNode.artifactPath
            ).then(res => {
                permissions = res
                this.deleteEnabled =
                    this.folibRepository.type !== 'group' &&
                    (hasRole('ARTIFACTS_MANAGER') ||
                        permissions.includes('ARTIFACTS_DELETE'))
            }).finally(() => {
                if (callback) callback()
            })
        },
    }
}
</script>
<style lang="scss">
.context-menu {
    z-index: 1000;
    background-color: #fff;
    border: 1px solid #d9d9d9;
    border-radius: 4px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);

    /deep/ .ant-menu-item {
        margin: 0;
        height: 35px;
        line-height: 35px;
        padding: 0 8px;
    }
}
</style>