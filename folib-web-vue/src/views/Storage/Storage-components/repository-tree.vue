/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div class="left_tree-container" ref="container">
        <div class="cover-box" v-if="isDragging"></div>
        <a-spin :spinning="loadingMore">
            <div ref="tree_container" class="tree_container" :style="{ height: topHeight + 'px' }"
                @scroll="handleScroll">
                <a-tree 
                    :key="key" 
                    class="repositoryTree" 
                    ref="tree" 
                    :replaceFields="replaceFields"
                    :load-data="(treeNode) => onLoadData(treeNode, false)" 
                    :tree-data="treeData" 
                    :show-line="true" 
                    @select="treeSelect"
                    @expand="(expandedKeys,obj) => onExpand(expandedKeys, obj, false)" 
                    @rightClick="(e) => rightClick(e,'tree')" 
                    :expandedKeys="expandedKeys" 
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
                                <a-icon class="tree_icon" style="margin-left: 5px;"
                                    v-if="type === 'dir' || type === 'DIR'"
                                    :type="expanded ? 'folder-open' : 'folder'" />
                                <a-icon class="tree_icon" style="margin-left: 10px;" v-else
                                    :type="getIconType(name, type)"></a-icon>
                                <span class="tree_title">
                                    {{ name }}
                                </span>
                            </span>
                        </div>
                    </template>
                </a-tree>
            </div>
        </a-spin>
        <div class="line" :class="isDragging ? 'line-drag' : ''" @mousedown="startDragging"></div>
        <a-spin :spinning="loadingMore">
            <div ref="tree_container" class="tree_container recycle" :style="{ height: bottomHeight + 'px' }"
                @scroll="handleScroll">
                <a-tree 
                    :key="recycleKey" 
                    class="repositoryTree" 
                    ref="tree" 
                    :replaceFields="replaceFields"
                    :load-data="(treeNode) => onLoadData(treeNode, true)" 
                    :tree-data="recycleTreeData" 
                    :show-line="true" @select="(key,e) => treeSelect(key,e,true)"
                    @expand="(expandedKeys,obj) => onExpand(expandedKeys,obj, true)" 
                    @rightClick="(e) => rightClick(e,'recycleTree')" 
                    :expandedKeys="expandedRecycleKeys" 
                    show-icon
                    :selectedKeys="selectRecycleKeys"
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
                                <a-icon class="tree_icon" style="margin-left: 5px;"
                                    v-if="type === 'dir' || type === 'DIR'"
                                    :type="expanded ? 'folder-open' : 'folder'" />
                                <a-icon class="tree_icon" style="margin-left: 8px;" v-else :style="type == 'recycle'? 'color:#393b3e':''"
                                    :type="getIconType(name, type)"></a-icon>
                                <span class="tree_title" :style="type == 'recycle'? 'color:#393b3e':''">
                                    {{ name }}
                                </span>
                            </span>
                        </div>
                    </template>
                </a-tree>
            </div>
        </a-spin>
        <rightMenu ref="rightMenu" v-show="showContextMenu" :style="contextMenuStyle" :folibRepository="folibRepository"
            :repositoryType="repositoryType" :currentFileDetial="currentFileDetial" :uploadEnabled="uploadEnabled"
            :copyEnabled='copyEnabled' :dispatchEnabled="dispatchEnabled" :moveEnabled="moveEnabled"
            :currentTreeNode="currentTreeNode" :isTrashView="isTrashView" @reload="reload" @localDelNode="localDelNode"
            @handleMenuClick="handleMenuClick" />
    </div>
</template>
<script>
import local from './images/local.svg'
import localCheck from './images/local-check.svg'
import remote from './images/remote.svg'
import remoteCheck from './images/remote-check.svg'
import virtual from './images/virtual.svg'
import virtualCheck from './images/virtual-check.svg'
import { getDockerArtifact, browse, getArtifact, getStorageAndRepositoryPermission, getArtifactPermission } from '@/api/folib'
import { getLayoutType } from '@/utils/layoutUtil'
import rightMenu from './right-menu.vue'
import { hasRole, isAdmin } from '@/utils/permission'

export default {
    components: {
        rightMenu
    },
    props: ['repositories', 'storageId'],
    data() {
        return {
            treeData: [],
            folibRepository: {},
            replaceFields: {
                key: 'key',
                title: 'name',
                children: 'children',
            },
            key: 0,
            repositoryType: '',
            artifactPath: '',
            loadingMore: false,
            selectedKeys: [],
            selectRecycleKeys: [],
            currentFileDetial: {},
            permissions: [],
            uploadEnabled: false,
            copyEnabled: false,
            dispatchEnabled: false,
            moveEnabled: false,
            showContextMenu: false,
            currentTreeNode: {},
            rightClickTop: '0px',
            rightClickLeft: '0px',
            enablUploadedLayout: ['Raw', 'php', 'Maven 2', 'npm', 'rpm', 'go', 'GitLfs', 'pub', 'debian'],
            expandedKeys: [],
            expandedRecycleKeys: [],
            topHeight: 500, // 初始顶部 div 的高度 (容器高度 - 底部高度 - 分隔条高度)
            bottomHeight: 40, // 初始底部 div 的高度
            isDragging: false, // 是否正在拖拽
            containerHeight: 0, // 容器的总高度
            recycleTreeData:[{
                name:'制品回收站',
                icon:'',
                type:'recycle',
                children:[],
                key:0
            }],
            isTrashView:false,
            recycleRepositryList:[],
            recycleKey:0
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
        getIconType() {
            return (name, type) => {
                const _name = name.toLowerCase()
                const _type = type.toLowerCase()
                let icon = ''
                if (_type === 'file') {
                    icon = 'file'
                }
                if (_name.indexOf('.png') !== -1) {
                    icon = 'file-image'
                }
                if (_name.indexOf('.zip') !== -1) {
                    icon = 'file-zip'
                }
                if (_name.indexOf('.md') !== -1) {
                    icon = 'file-markdown'
                }
                if (_name.indexOf('.pdf') !== -1) {
                    icon = 'file-pdf'
                }
                if(_type === 'recycle'){
                    icon = 'delete'
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
        treeClickKey() {
            return this.$store.state.treeClickKey
        }
    },
    watch: {
        repositories: {
            handler(val) {
                if (val) {
                    this.isTrashView = false
                    this.loadingMore = false
                    this.loadingMoreShow(false)
                    this.treeData = this.treeData.concat(JSON.parse(JSON.stringify(val)))
                    this.recycleRepositryList = this.recycleRepositryList.concat(JSON.parse(JSON.stringify(val)))
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
                    const objs = ['treeData','recycleRepositryList']
                    objs.forEach(key => {
                        this[key].forEach(ele => {
                            ele.fileType = 'document'
                            ele.key = ele.id
                            ele.name = ele.id
                            ele.artifactPath = ''
                            ele.newDetailPage = true
                            ele.treeType = 'root'
                        })
                    })
                    if (!this.treeData.length) {
                        return
                    }
                    this.getRecycleTreeData()
                    this.selectedKeys = [this.treeData[0].id]
                    const e = {
                        node: {
                            dataRef: this.treeData[0]
                        }
                    }
                    this.treeSelect('', e)
                }
            },
            immediate: true
        },
        storageId(val) {
            if (val) {
                // 切换不同的存储空间时，清空原有的树结构
                this.key++
                this.recycleKey ++
                this.empty()
                this.loadingMoreShow(true)
            }
        },
        // isTrashView() {
        //     this.reload()
        // },
    },
    mounted() {
        document.addEventListener('click', this.closeContextMenu)
        this.$nextTick(() => {
            this.getPosition()
        })
        this.recycleTreeData = [{
            name:'制品回收站',
            icon:'',
            type:'recycle',
            children:[],
            key:this.storageId
        }]
    },
    beforeDestroy() {
        document.removeEventListener('click', this.closeContextMenu)
    },
    methods: {
        reload(key = false) {
            this.recycleKey ++
            if(key){
                this.getRecycleTreeData()
            }
            if(!key){
                this.recycleRepositryList = []
                this.treeData = []
                this.$emit('getDetailInfo', true, 'repositoryTree')
            }
        },
        // 远程节点删除成功后，调用本地删除节点方法（实现删除后树表展开结构不变）
        localDelNode(tarNode) {
            const { storageId, repositoryId, artifactPath,type } = tarNode
            console.log(storageId, repositoryId, artifactPath)
            const arr = artifactPath.split('/')
            const length = arr.length
            const keyToDelete = arr[length - 1]
            // 从 treeData 中递归删除节点
            const removeNode = (nodes, key) => {
                return nodes.filter((node) => {
                    if (node.name == key) return false // 移除目标节点
                    if (node.children) {
                        node.children = removeNode(node.children, key)
                    }
                    return true
                })
            }
            this.treeData = removeNode(this.treeData, keyToDelete)
            this.reload(true)
        },
        // 右键菜单选择操作
        handleMenuClick(active) {
            this.$emit('handleMenuClick', active, this.currentTreeNode)
        },
        setKeyValue() {
            this.key++
        },
        // 鼠标右键
        rightClick(e,type) {
            this.isTrashView = type === 'recycleTree'
            const { treeType } = e.node.dataRef
            if (treeType !== 'root' && treeType !== 'lastRoot') {
                this.showContextMenu = true
                this.rightClickTop = `${e.event.clientY}px`;
                this.rightClickLeft = `${e.event.clientX}px`;
                this.currentTreeNode = e.node.dataRef;
                this.$refs.rightMenu.handlerDataPermission(e.node.dataRef,type)
            }
        },
        closeContextMenu() {
            this.showContextMenu = false;
        },
        empty() {
            this.treeData = []
            this.recycleRepositryList = []
            this.recycleTreeData = [{
                name:'制品回收站',
                icon:'',
                type:'recycle',
                children:[],
                key:this.storageId
            }]
            this.expandedRecycleKeys = []
            this.getPosition()
        },
        // 设置loading状态
        loadingMoreShow(key) {
            this.loadingMore = key
            // 设置只有在请求结束后才可以切换模式，方式快速切换导致数据渲染错误
            this.$store.commit('setSwitchDisabled', key)
        },
        // 滚动条滚动
        handleScroll(event) {
            const { scrollTop, clientHeight, scrollHeight } = event.target;
            // 当滚动到底部时加载更多
            if (scrollTop + clientHeight >= scrollHeight) {
                const total = this.treeData.length
                this.$emit('loadMore', total)
            }
        },
        // 判断那些文件类型是可以打开的
        getFileIsOpen(name) {
            const _name = name.toLowerCase()
            const tarArr = ['.tar', '.jar', '.zip', '.7z', '.tar.gz', 'tgz']
            let key = false
            tarArr.forEach(ele => {
                if (_name.indexOf(ele) !== -1) {
                    key = true
                }
            })
            return key
        },
        // 树节点点击
        treeSelect(key, e, isRecycle) {
            const { newDetailPage } = e.node.dataRef
            if(isRecycle){
                this.selectRecycleKeys = [e.node.dataRef.key]
            }else{
                this.selectedKeys = [e.node.dataRef.key]
            }
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
            } else {
                let data = null
                // 获取当前子节点的最顶层父节点（仓库节点）
                this.treeData.forEach(ele => {
                    if (ele.id === e.node.dataRef.repositoryId) {
                        data = ele
                    }
                })
                this.folibRepository = data
                // this.$emit('repositorySelect', data) 
                this.$nextTick(() => {
                    setTimeout(() => {
                        e.isRecycle = isRecycle
                        this.$emit('treeSelect', key, e, data)
                    }, 0);
                })
            }
            // 如果为新页面（jar包下面的文件夹），更新右侧展示页面数据
            if (!!newDetailPage) {
                const { id, storageId } = this.folibRepository
                let params = e.node.dataRef
                params.repositoryId = id
                params.storageId = storageId
                this.$store.commit('setCurrentTreeNode', params)
            }
        },
        // 获取当前已展开节点的key
        onExpand(expandedKeys,obj,key) {
            if(key){
                this.expandedRecycleKeys = expandedKeys
            }else{
                this.expandedKeys = expandedKeys
            }
            if(key && obj.node.dataRef.name === '制品回收站'){ // 回收站打开
                this.getPosition(obj.expanded ? 320 : 40)
            }
        },
        getRecycleTreeData(){
            let list = []
            this.recycleRepositryList.forEach(ele => {
                list.push(JSON.parse(JSON.stringify(ele)))
            })
            this.recycleTreeData[0].children = [...list]
            this.recycleTreeData = [...this.recycleTreeData]
        },
        // 懒加载获取节点
        onLoadData(treeNode,isTrashView) {
            const nowDataKey = isTrashView ? 'recycleTreeData' : 'treeData'
            if(treeNode.dataRef.type === 'recycle'){
                this.getPosition(320)
            }
            if (treeNode.dataRef.fileType === 'document') {
                this.folibRepository = treeNode.dataRef
                this.repositoryType = getLayoutType(this.folibRepository)
                this.queryPermission()
            }
            const { storageId, id, layout } = this.folibRepository
            const { artifactPath, name } = treeNode.dataRef
            const params = {
                treeNode,
                storageId,
                id,
                layout,
                artifactPath,
                name
            }
            if (this.getFileIsOpen(name)) {
                return this.getPackagePreview(params,nowDataKey)
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
                            if (res.directories.some(ele => ele.name === '.trash')) {
                                if (isTrashView) {
                                    f = res.directories.filter(ele => ele.name === '.trash')
                                } else {
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
                        if (res.files.length > 0 && !isTrashView) {
                            const a = res.files
                            a.forEach((item, index, a) => {
                                item.isLeaf = !this.getFileIsOpen(item.name)
                                item.type = 'file'
                                item.key = id + item.artifactPath
                                treeNode.dataRef.children.push(item)
                            })
                        }
                        this[nowDataKey] = [...this[nowDataKey]]
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
                        if (res.directories.some(ele => ele.name === '.trash')) {
                            if (isTrashView) {
                                f = res.directories.filter(ele => ele.name === '.trash')
                            } else {
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
                    if (res.files.length > 0 && !isTrashView) {
                        const a = res.files
                        a.forEach((item, index, a) => {
                            item.isLeaf = !this.getFileIsOpen(item.name)
                            item.type = 'file'
                            item.key = id + item.artifactPath
                        })
                        treeNode.dataRef.children = treeNode.dataRef.children.concat(a)
                    }

                    this[nowDataKey] = [...this[nowDataKey]]
                    resolve()
                })
            })
        },
        // 获取可以继续打开的文件的目录（对应包预览）
        getPackagePreview({ treeNode, storageId, id, artifactPath },nowDataKey) {
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
                    function setNewDetailPage(arr) {
                        arr.forEach(ele => {
                            ele.newDetailPage = true
                            ele.treeType = 'lastRoot'
                            ele.storageId = storageId
                            ele.repositoryId = id
                            ele.key = ele.name
                            ele.artifactPath = `${id}/${artifactPath}/${ele.name}`
                            if (ele?.children?.length) {
                                setNewDetailPage(ele.children)
                            }
                        })
                    }
                    treeNode.dataRef.children = []
                    if (res.listTree) {
                        setNewDetailPage(res.listTree)
                        treeNode.dataRef.children = treeNode.dataRef.children.concat(res.listTree)
                    }
                    this[nowDataKey] = [...this[nowDataKey]]
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
            })
        },
        startDragging(event) {
            event.preventDefault()
            this.isDragging = true;
            document.addEventListener("mousemove", this.onMouseMove);
            document.addEventListener("mouseup", this.stopDragging);
        },
        onMouseMove(event) {
            if (!this.isDragging) return;

            const containerRect = this.$el.getBoundingClientRect();
            const offsetY = event.clientY - containerRect.top;

            // 限制顶部和底部高度不能小于 40px
            const newTopHeight = Math.max(40, offsetY);
            const newBottomHeight = Math.max(
                40,
                this.containerHeight - newTopHeight - 5 // 计算底部 div 高度
            );

            // 如果顶部和底部高度都满足条件，则更新高度
            if (newTopHeight + newBottomHeight + 5 === this.containerHeight) {
                this.topHeight = newTopHeight;
                this.bottomHeight = newBottomHeight;
            }
        },
        stopDragging() {
            this.isDragging = false;
            document.removeEventListener("mousemove", this.onMouseMove);
            document.removeEventListener("mouseup", this.stopDragging);
        },
        getPosition(bottomHeight = 40){
            this.bottomHeight = bottomHeight
            this.containerHeight = this.$refs.container.clientHeight - 5
            this.topHeight = this.containerHeight - this.bottomHeight - 5
        }
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
<style lang="scss">
.repositoryTree.ant-tree li {
    padding: 2px 0px !important;
}

.repositoryTree .ant-tree-node-content-wrapper {
    width: 90% !important;
    height: 32px !important;
    line-height: 32px !important;
    border-radius: 8px !important;
}

.repositoryTree .ant-tree-switcher_close,
.ant-tree-switcher_open {
    height: 32px !important;
    line-height: 32px !important;
}

.tree_container {
    margin-left: 12px;
    margin-right: 12px;
    padding-top: 0px;
    overflow: hidden;

    &:hover {
        overflow: auto;
    }
}

.repositoryTree .ant-tree-switcher-noop {
    display: none !important;
}

.repositoryTree .tree_icon {
    font-size: 16px;
    font-weight: 500;
}
</style>

<style lang="less" scoped>
@import url('./repository-tree.less');
</style>
<style lang="scss">
.left_tree-container {
    position: relative;
    height: calc(100vh - 282px);
    display: flex;
    flex-direction: column;

    .cover-box {
        height: 100%;
        width: 100%;
        position: absolute;
        cursor: s-resize;
        left: 0;
        top: 0;
        z-index: 10;
    }

    .line {
        position: relative;
        z-index: 11;
        height: 5px;
        transition: all 0.5s;
        background: transparent;
        transition-delay: 0.2s;
        border-bottom: 1px solid #ccc;
        cursor: s-resize;

        &:hover {
            border-bottom: 1px solid transparent;
            background: #1890ff;
        }

        &.line-drag {
            border-bottom: 1px solid transparent;
            background: #1890ff;
        }
    }
}

.recycle {
    margin-top: 5px;
}
</style>
