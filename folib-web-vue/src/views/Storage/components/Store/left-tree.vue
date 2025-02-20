/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div ref="container" class="left_tree_container">
        <div class="cover-box" v-if="isDragging"></div>
        <div ref="tree_container_sty" class="tree_container_sty" :style="{ height: topHeight + 'px' }">
            <vue-easy-tree
                :props="{
                    label: 'name',
                    children: 'children',
                    isLeaf: 'isLeaf'
                }"
                lazy
                :data="treeData"
                class="leftTree"
                :height="`${topHeight}px`"
                node-key="artifactPath"
                :load="(treeNode, resolve) => onLoadData(treeNode,resolve, false)"
                @select="(key,e)=>treeSelect(key,e,false)"
                @node-contextmenu="onRightClick"
                @node-expand="(expandedKeys, treeNode) => onExpand(expandedKeys, treeNode, false)"
                :selectedKeys="selectedKeys"
                :expandedKeys="expandedKeys"
            >
                <template slot-scope="{data,node}">
                    <div class="title_box">
                        <span>
                            <a-icon v-if="node.loading && !node.expanded" type="loading" :style="{color: '#1890ff'}"/>
                            <a-icon class="tree_icon" style="margin-left: 5px;"
                                    v-if="data.type === 'dir' || data.type === 'DIR'"
                                    :type="node.expanded ? 'folder-open' : 'folder'" />
                            <a-icon class="tree_icon" style="margin-left: 8px;" v-else :style="data.type === 'recycle'? 'color:#393b3e':''"
                                    :type="getIconType(data.name, data.type)"></a-icon>
                            <span class="tree_title" :style="data.type === 'recycle'? 'color:#393b3e':''">
                                {{ data.name }}
                            </span>
                        </span>
                    </div>
                </template>
            </vue-easy-tree>
        </div>
        <div class="line-box" :class="isDragging ? 'line-drag' : ''" @mousedown="startDragging"></div>
        <div ref="tree_container_sty" class="tree_container_sty recycle" :style="{ height: bottomHeight + 'px' }">
            <vue-easy-tree
                :props="{
                    label: 'name',
                    children: 'children',
                    isLeaf: 'isLeaf'
                }"
                :data="trashData"
                class="leftTree"
                :height="`${bottomHeight}px`"
                node-key="artifactPath"
                :load-data="(treeNode,resolve) => onLoadData(treeNode, resolve, true)"
                @select="(key,e)=>treeSelect(key,e,true)"
                @node-contextmenu="onRightClick"
                @node-expand="(expandedKeys, treeNode) => onExpand(expandedKeys, treeNode, true)"
                :selectedKeys="selectRecycleKeys"
                :expandedKeys="expandedRecycleKeys"
            >
                <template slot-scope="{data,node}">
                    <div class="title_box">
                        <span>
                            <a-icon v-if="node.loading && !node.expanded" type="loading" :style="{color: '#1890ff'}"/>
                            <a-icon class="tree_icon" style="margin-left: 5px;"
                                    v-if="data.type === 'dir' || data.type === 'DIR'"
                                    :type="node.expanded ? 'folder-open' : 'folder'" />
                            <a-icon class="tree_icon" style="margin-left: 8px;" v-else :style="data.type === 'recycle'? 'color:#393b3e':''"
                                    :type="getIconType(data.name, data.type)"></a-icon>
                            <span class="tree_title" :style="data.type === 'recycle'? 'color:#393b3e':''">
                                {{ data.name }}
                            </span>
                        </span>
                    </div>
                </template>
            </vue-easy-tree>
        </div>
    </div>
</template>

<script>
import VueEasyTree from "@wchbrad/vue-easy-tree"
import "@wchbrad/vue-easy-tree/src/assets/index.scss"
export default {
    components: {
        VueEasyTree
    },
    props: ['trashData', 'treeData'],
    data() {
        return {
            topHeight: 500, // 初始顶部 div 的高度 (容器高度 - 底部高度 - 分隔条高度)
            bottomHeight: 40, // 初始底部 div 的高度
            isDragging: false, // 是否正在拖拽
            containerHeight: 0, // 容器的总高度
            expandedKeys:[],
            selectedKeys:[],
            expandedRecycleKeys:[],
            selectRecycleKeys:[],
            testData: []
        }
    },
    computed: {
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
    },
    mounted() {
        this.$nextTick(() => {
            this.getPosition()
        })
    },
    methods: {
        onLoadData(treeNode,resolve, isTrashView) {
            treeNode.loading = true
            this.$emit('onLoadData', treeNode, isTrashView, resolve);
        },
        onExpand(expandedKeys, node, expanded){
            if(node.data.name === '.trash'){
                this.getPosition(expanded ? 300 : 40)
            }
            // if(key){
            //     this.expandedRecycleKeys = expandedKeys
            // }else{
            //     this.expandedKeys = expandedKeys
            // }
        },
        treeSelect(key, e, type) {
            if(type){
                if(this.expandedRecycleKeys.length){
                    this.expandedRecycleKeys = []
                    this.getPosition()
                }else{
                    this.expandedRecycleKeys = ['.trash']
                    this.getPosition(300)
                }
                this.selectRecycleKeys = key
                this.selectedKeys = []
            }else{
                this.selectedKeys = key
                this.selectRecycleKeys = []
            }
            this.$emit('treeSelect',key, e)
        },
        onRightClick(event, data) {
            this.$emit('onRightClick',event, data)
        },
        startDragging(event) {
            event.preventDefault()
            this.isDragging = true
            document.addEventListener("mousemove", this.onMouseMove)
            document.addEventListener("mouseup", this.stopDragging)
        },
        onMouseMove(event) {
            if (!this.isDragging) return

            const containerRect = this.$el.getBoundingClientRect()
            const offsetY = event.clientY - containerRect.top

            // 限制顶部和底部高度不能小于 40px
            const newTopHeight = Math.max(40, offsetY)
            const newBottomHeight = Math.max(
                40,
                this.containerHeight - newTopHeight - 5 // 计算底部 div 高度
            )

            // 如果顶部和底部高度都满足条件，则更新高度
            if (newTopHeight + newBottomHeight + 5 === this.containerHeight) {
                this.topHeight = newTopHeight;
                this.bottomHeight = newBottomHeight;
            }
        },
        stopDragging() {
            this.isDragging = false
            document.removeEventListener("mousemove", this.onMouseMove);
            document.removeEventListener("mouseup", this.stopDragging);
        },
        getPosition(bottomHeight = 40) {
            if(bottomHeight > this.bottomHeight || bottomHeight == 40){
                this.bottomHeight = bottomHeight
            }
            this.containerHeight = this.$refs.container.clientHeight - 5
            this.topHeight = this.containerHeight - this.bottomHeight - 5
        },
    }
}
</script>
<style lang="scss">
.left_tree_container {
    position: relative;
    height: calc(100vh - 383px);
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

    .line-box {
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

.tree_container_sty {
    margin-left: 12px;
    margin-right: 12px;
    padding-top: 0px;
    overflow: hidden;

    &:hover {
        overflow: auto;
    }
}
</style>
<style lang="less" scoped>
@import url('./left-tree.less');
</style>
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
.leftTree.ant-tree li {
    padding: 2px 0px !important;
}

.leftTree .ant-tree-node-content-wrapper {
    width: 95% !important;
    height: 32px !important;
    line-height: 32px !important;
    border-radius: 8px !important;
}

.leftTree .ant-tree-switcher_close,
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

.leftTree .ant-tree-switcher-noop {
    display: none !important;
}

.leftTree .tree_icon {
    font-size: 16px;
    font-weight: 500;
}
</style>