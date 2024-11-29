/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div>
        <div ref="tree_container" class="tree_container" @scroll="handleScroll">
            <a-tree 
                :key="key"
                class="repositoryTree"
                ref="tree"
                :replaceFields="replaceFields" 
                :load-data="onLoadData" 
                :tree-data="treeData" 
                :show-line="true"
                @select="treeSelect" 
                @expand="onExpand" 
                @rightClick="rightClick"
                show-icon
                :selectedKeys="selectedKeys"
            >
                <a-icon slot="switcherIcon" type="down" />
                <a-icon slot="switcherIcon" type="folder-open" />
                <template slot="title" slot-scope="{ expanded,name,id,type,selected,fileType }">
                    <div class="title_box">
                        <img v-if="fileType === 'document'" :src="getSrc(selected, type)" alt="" width="24">
                        <span v-if="fileType === 'document'" class="tree_title">
                            {{ id }}
                        </span>
                        <span v-else>
                            <a-icon class="tree_icon" style="margin-left: 5px;" v-if="type === 'dir' || type === 'DIR'" :type="expanded ? 'folder-open' : 'folder'" />
                            <a-icon class="tree_icon" style="margin-left: 10px;" v-else :type="getIconType(name,type)"></a-icon>
                            <span class="tree_title">
                                {{ name }}
                            </span>
                        </span>
                    </div>
                </template>
            </a-tree>
        </div>
        <div>
            <!-- :tip="`${$t('Storage.Loading')}...`" -->
            <a-spin style="width:100%;" v-if="loadingMore"/>
        </div>
        <!-- v-if='showContextMenu'  -->
        <rightMenu
            v-show="showContextMenu"
            :style="contextMenuStyle"
            :folibRepository="folibRepository" 
            :repositoryType="repositoryType" 
            :currentFileDetial="currentFileDetial" 
            :uploadEnabled="uploadEnabled"
            :copyEnabled='copyEnabled'
            :dispatchEnabled="dispatchEnabled"
            :moveEnabled="moveEnabled"
            :deleteEnabled="deleteEnabled"
            :currentTreeNode="currentTreeNode"
            :isTrashView="isTrashView"
            @reload="reload"
            @handleMenuClick="handleMenuClick"
        />
    </div>
</template>
<script>
import local from './images/local.svg'
import localCheck from './images/local-check.svg'
import remote from './images/remote.svg'
import remoteCheck from './images/remote-check.svg'
import virtual from './images/virtual.svg'
import virtualCheck from './images/virtual-check.svg'
import { getDockerArtifact, browse, getArtifact, getStorageAndRepositoryPermission } from '@/api/folib'
import { getLayoutType } from '@/utils/layoutUtil'
import rightMenu from './right-menu.vue'
import { hasRole, isAdmin } from '@/utils/permission'
export default {
    components:{
        rightMenu
    },
    props: ['repositories','storageId','isTrashView'],
    data() {
        return {
            treeData: [],
            folibRepository:{},
            replaceFields:{
                key: 'key',
                title: 'name',
                children: 'children',
            },
            key:0,
            repositoryType:'',
            artifactPath:'',
            loadingMore:false,
            selectedKeys:[],
            currentFileDetial:{},
            permissions:[],
            uploadEnabled: false,
            copyEnabled: false,
            dispatchEnabled: false,
            moveEnabled: false,
            deleteEnabled: false,
            showContextMenu: false,
            currentTreeNode:{},
            rightClickTop:'0px',
            rightClickLeft:'0px',
            enablUploadedLayout: ['Raw', 'php', 'Maven 2', 'npm', 'rpm', 'go','GitLfs', 'pub','debian'],
        };
    },
    computed: {
        getSrc() {
            return (selected, type) => {
                let src = ''
                if (type === 'hosted') {
                    src = selected ? localCheck : local
                } else if (type === 'group') {
                    src = selected ? virtualCheck : virtual
                } else {
                    src = selected ? remoteCheck : remote
                }
                return src
            }
        },
        getIconType(){
            return (name,type) => {
                const _name = name.toLowerCase()
                const _type = type.toLowerCase()
                let icon = ''
                if (_type === 'file') {
                    icon = 'file'
                }
                if(_name.indexOf('.png') !== -1){
                    icon = 'file-image'
                }
                if(_name.indexOf('.zip') !== -1){
                    icon = 'file-zip'
                }
                if(_name.indexOf('.md') !== -1){
                    icon = 'file-markdown'
                }
                if(_name.indexOf('.pdf') !== -1){
                    icon = 'file-pdf'
                }
                return icon
            }
        },
        contextMenuStyle() {
            return {
                position: 'fixed',
                top: this.rightClickTop,
                left: this.rightClickLeft,
            }
        },
    },
    watch: {
        repositories: {
            handler(val) {
                if (val) {
                    this.loadingMore = false
                    this.treeData = this.treeData.concat(JSON.parse(JSON.stringify(val)))
                    // function uniqueById(arr) {
                    //     const unique = [];
                    //     const seen = new Set();
                    //     arr.forEach(item => {
                    //         if (!seen.has(item.id)) {
                    //         unique.push(item);
                    //         seen.add(item.id);
                    //         }
                    //     });
                    //     return unique;
                    // }
                    // this.treeData = uniqueById(this.treeData)
                    this.treeData.map(ele => {
                        ele.fileType = 'document'
                        ele.key = ele.id
                        ele.name = ele.id
                        ele.artifactPath = ''
                        ele.newDetailPage = true
                        ele.treeType = 'root'
                    })
                    if(!this.treeData.length){
                        return
                    }
                    this.selectedKeys = [this.treeData[0].id]
                    const e = {
                        node: {
                            dataRef: this.treeData[0]
                        }
                    }
                    this.treeSelect('',e)
                }
            },
            immediate: true
        },
        storageId(val){
            if(val){
                // 切换不同的存储空间时，清空原有的树结构
                this.key ++ 
                this.empty()
                this.loadingMoreShow(true)
            }
        },
        isTrashView(){
            this.reload()
        }
    },
    mounted() {
        document.addEventListener('click', this.closeContextMenu);
    },
    beforeDestroy() {
        document.removeEventListener('click', this.closeContextMenu);
    },
    methods: {
        reload(){
            this.treeData = []
            this.$emit('getDetailInfo',true,'repositoryTree')
        },
        // 右键菜单选择操作
        handleMenuClick(active){
            this.$emit('handleMenuClick',active,this.currentTreeNode)
        },
        setKeyValue(){
            this.key ++
        },
        rightClick(e){
            const {treeType} = e.node.dataRef
            if(treeType !== 'root' && treeType !== 'lastRoot'){
                this.showContextMenu = true
                this.rightClickTop = `${e.event.clientY}px`;
                this.rightClickLeft = `${e.event.clientX}px`;
                this.currentTreeNode = e.node.dataRef;
            }
        },
        closeContextMenu() {
            this.showContextMenu = false;
        },
        empty(){
            this.treeData = []
        },
        // 设置loading状态
        loadingMoreShow(key){
            this.loadingMore = key
            console.log(this.loadingMore)
        },
        handleScroll(event){
            const { scrollTop, clientHeight, scrollHeight } = event.target;
            // 当滚动到底部时加载更多
            if (scrollTop + clientHeight >= scrollHeight) {
                const total = this.treeData.length
                this.$emit('loadMore', total)
            }
        },
        // 判断那些文件类型是可以打开的
        getFileIsOpen(name){
            const _name = name.toLowerCase()
            const tarArr = ['.tar','.jar','.zip','.7z','.tar.gz','tgz']
            let key = false
            tarArr.forEach(ele => {
                if(_name.indexOf(ele) !== -1){
                    key = true
                }
            })
            return key
        },
        treeSelect(key, e) {
            const {newDetailPage} = e.node.dataRef
            this.selectedKeys = [e.node.dataRef.key]
            this.$store.commit('setNewDetailPage', !!newDetailPage)
            if (e.node.dataRef.fileType == 'document') {
                this.folibRepository = e.node.dataRef
                this.repositoryType = getLayoutType(this.folibRepository)
                if (this.folibRepository.status.indexOf('Out of Service') !== -1) {
                    this.$notification.warning({
                        message: this.$t('Store.ServiceShutdown')
                    })
                    return false
                }
                if (!this.folibRepository.allowsDirectoryBrowsing) {
                    this.$notification.warning({
                        message: this.$t('Store.BrowseNotEnabled')
                    })
                    return false
                }
                this.$emit('repositorySelect', e.node.dataRef)
            }else{
                this.$emit('treeSelect', key, e)
            }
            if(!!newDetailPage){
                console.log(e.node.dataRef)
                const {id, storageId} = this.folibRepository
                let params = e.node.dataRef
                params.repositoryId = id
                params.storageId = storageId
                this.$store.commit('setCurrentTreeNode', params)
            }
        },
        onExpand() {
            this.$emit('onExpand')
        },
        onLoadData(treeNode) {
            console.log(treeNode,'treeNodetreeNodetreeNode')
            if(treeNode.dataRef.fileType === 'document'){
                this.folibRepository = treeNode.dataRef
                this.repositoryType = getLayoutType(this.folibRepository)
                this.queryPermission()
            }
            const {storageId,id,layout} = this.folibRepository
            const {artifactPath,name} = treeNode.dataRef
            const params = {
                treeNode,
                storageId,
                id,
                layout,
                artifactPath,
                name
            }
            if(this.getFileIsOpen(name)){
                return this.getPackagePreview(params)
            }
            
            if (layout === 'Docker') {
                return new Promise(resolve => {
                    if (treeNode.dataRef.children) {
                        resolve()
                        return
                    }
                    getDockerArtifact(
                        storageId,
                        id,
                        artifactPath
                    ).then(res => {
                        treeNode.dataRef.children = []
                        if (res.directories.length > 0) {
                            let f = res.directories
                            if(res.directories.some(ele => ele.name === '.trash')){
                                if(this.isTrashView){
                                    f = res.directories.filter(ele => ele.name === '.trash')
                                }else{
                                    f = res.directories.filter(ele => ele.name !== '.trash')
                                }
                            }
                            const d = f
                            d.forEach((item, index, d) => {
                                item.type = 'dir'
                                item.key = id + item.artifactPath
                                treeNode.dataRef.children.push(item)
                            })
                        }
                        if (res.files.length > 0 && !this.isTrashView) {
                            const a = res.files
                            a.forEach((item, index, a) => {
                                item.isLeaf = !this.getFileIsOpen(item.name)
                                item.type = 'file'
                                item.key = id + item.artifactPath
                                treeNode.dataRef.children.push(item)
                            })
                        }
                        this.treeData = [...this.treeData]
                        resolve()
                    })
                })
            }

            return new Promise(resolve => {
                if (treeNode.dataRef.children) {
                    resolve()
                    return
                }
                browse(
                    storageId,
                    id,
                    artifactPath
                ).then(res => {
                    if (!treeNode.dataRef.children) {
                        treeNode.dataRef.children = []
                    }
                    if (res.directories.length > 0) {
                        let f = res.directories
                        if(res.directories.some(ele => ele.name === '.trash')){
                            if(this.isTrashView){
                                f = res.directories.filter(ele => ele.name === '.trash')
                            }else{
                                f = res.directories.filter(ele => ele.name !== '.trash')
                            }
                        }
                        const d = f
                        d.forEach((item, index, d) => {
                            item.type = 'dir'
                            item.key = id + item.artifactPath
                        })
                        treeNode.dataRef.children = d
                    }
                    if (res.files.length > 0 && !this.isTrashView) {
                        const a = res.files
                        a.forEach((item, index, a) => {
                            item.isLeaf = !this.getFileIsOpen(item.name)
                            item.type = 'file'
                            item.key = id + item.artifactPath
                        })
                        treeNode.dataRef.children = treeNode.dataRef.children.concat(a)
                    }

                    this.treeData = [...this.treeData]
                    resolve()
                })
            })
        },
        // 获取可以继续打开的文件的目录（对应包预览）
        getPackagePreview({treeNode,storageId,id,artifactPath}){
            return new Promise(resolve => {
                if (treeNode.dataRef.children) {
                    resolve()
                    return
                }
                getArtifact(
                    this.repositoryType,
                    storageId,
                    id,
                    artifactPath
                ).then(res => {
                    this.currentFileDetial = res
                    function setNewDetailPage(arr){
                        arr.forEach(ele => {
                            ele.newDetailPage = true
                            ele.treeType = 'lastRoot'
                            ele.key = ele.name
                            ele.artifactPath = `${id}/${artifactPath}/${ele.name}`
                            if(ele?.children?.length){
                                setNewDetailPage(ele.children)
                            }
                        })
                    }
                    treeNode.dataRef.children = []  
                    if(res.listTree){
                        setNewDetailPage(res.listTree)
                        treeNode.dataRef.children = treeNode.dataRef.children.concat(res.listTree)
                    }
                    this.treeData = [...this.treeData]
                    resolve()
                })
            })
        },
        queryPermission() {
            this.storageAdmin = ""
            this.permissions = []
            getStorageAndRepositoryPermission(
                this.folibRepository.storageId,
                this.folibRepository.id
            ).then(res => {
                this.storageAdmin = res.storageAdmin
                this.permissions = res.permissions || []
                this.uploadEnabled =
                this.folibRepository.status.indexOf('Out of Service') === -1 &&
                this.enablUploadedLayout.includes(this.folibRepository.layout) &&
                (this.folibRepository.type === 'hosted' || (this.folibRepository.type === 'group' && this.folibRepository.groupDefaultRepository)) &&
                (hasRole('ARTIFACTS_MANAGER') ||
                this.permissions.includes('ARTIFACTS_DEPLOY'))
                this.copyEnabled = this.folibRepository.type === 'hosted' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_COPY'))
                this.dispatchEnabled = this.folibRepository.type === 'hosted' && isAdmin()
                this.moveEnabled = this.folibRepository.type === 'hosted' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_MOVE'))
                this.deleteEnabled = this.folibRepository.type !== 'group' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_DELETE'))
            })
        },
    },
};
</script>
<style lang="scss" scoped>
.title_box {
    width: 100%;
    display: flex;
    align-items: center
}

.tree_title {
    margin-left: 5px;
    font-size: 16px;
    color: #67748e;
}
</style>
<style>
.repositoryTree .ant-tree-node-content-wrapper {
    width: 90% !important;
    height: 32px !important;
    line-height: 32px !important;
}

.repositoryTree .ant-tree-switcher_close,
.ant-tree-switcher_open {
    height: 32px !important;
    line-height: 32px !important;
}
.tree_container{
    max-height: 650px;
    overflow: auto;
}
.repositoryTree .ant-tree-switcher-noop{
    display: none !important;
}

.repositoryTree .tree_icon{
    font-size: 16px;
    font-weight: 500;
}
</style>

<style lang="less" scoped>
    @import url('./repository-tree.less');
</style>
