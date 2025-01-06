/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div class='storage_list'>
        <a-anchor :targetOffset="navbarFixed ? 100 : 10" :affix="false">
            <a-anchor-link 
                v-for="(item, index) in storageData" 
                :key="index" 
                href="javascript:void(null)"
                :class="{ slectActive: item.id === currentStorage.id }"
            >
                <div slot="title" class="ant-list-item-meta" @click="setCurrentStorage(item)">
                    <a-icon 
                        :type="item.storageProvider === 's3' ? 'cloud' : 'appstore'" 
                        theme="filled"
                        class="text-gray-6 text-lg" 
                    />
                    <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">{{ item.id }}</span>
                    </h4>
                </div>
            </a-anchor-link>
        </a-anchor>
    </div>
</template>

<script>
import virtualScroll from 'vue-virtual-scroll-list'
export default {
    components:{
        virtualScroll
    },
    props:{
        navbarFixed: {
            type: Boolean,
            default: true,
        },
        storageData:{
            type:Array,
            default:() => {
                return []
            }
        },
        currentStorage:{
            type:Object,
            default:() => {
                return {}
            }
        }
    },
    data() {
        return {
            
        }
    },
    mounted() {
        
    },
    methods:{
        setCurrentStorage(item){
            this.$emit('setCurrentStorage',item)
        },
    }
}
</script>
<style lang="scss" scoped>
.slectActive {
  background-color: #eeeeee !important;
  border-radius: 8px;
}
.storage_list{
    height: calc(100vh - 205px);
    overflow-y: auto;
    padding:12px;
    padding-top: 0;
}
</style>