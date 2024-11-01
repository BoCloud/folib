<!-- 
	This is the main page of the application, the layout component is used here,
	and the router-view is passed to it.
	Layout component is dynamically declared based on the layout for each route,
	specified in routes list router/index.js .
 -->

<template>
    <div id="app">
        <a-locale-provider :locale="locale">
            <component :is="layout" v-if="routerAlive">
                <router-view/>
            </component>
        </a-locale-provider>
    </div>
</template>

<script>
import { ACCESS_TOKEN, USER_INFO } from '@/store/mutation-types'
import zhCN from 'ant-design-vue/lib/locale-provider/zh_CN'
import enUS from 'ant-design-vue/lib/locale-provider/en_US'

export default ({
    provide() {
        return {
            reload: this.reload,
        }
    },
    computed: {
        // Sets components name based on current route's specified layout, defaults to
        // <layout-default></layout-default> component.
        layout() {
            return "layout-" + (this.$route.meta.layout || "default").toLowerCase();
        },
        // Sets locale based on current route's specified locale, defaults to zhCN.
        locale() {
            return this.$store.state.language.lang === 'zh' ? zhCN : enUS;
        },
    },
    created() {

        // 在页面加载时读取sessionStorage里的状态信息
        if (localStorage.getItem(USER_INFO) && localStorage.getItem(ACCESS_TOKEN)) {
            this.$store.replaceState(
                Object.assign(
                    {},
                    this.$store.state,
                    JSON.parse(localStorage.getItem(USER_INFO))
                )
            )
        }

        // 在页面刷新时将vuex里的信息保存到sessionStorage里
        // beforeunload事件在页面刷新时先触发
        window.addEventListener('beforeunload', () => {
            if (this.$store.state && localStorage.getItem(ACCESS_TOKEN)) {
                localStorage.setItem(USER_INFO, JSON.stringify(this.$store.state))
            }
        })

        this.storageExpireLocal()
    },
    data() {
        return {routerAlive: true}
    },
    methods: {
        reload() {
            this.routerAlive = false
            this.$nextTick(function () {
                this.routerAlive = true
            })
        },
        storageExpireLocal() {
            // 存值函数
            // 接收三个参数：键、值、有效天数
            Storage.prototype.setCanExpireLocal = (key, value, expire) => {
                // 判断传入的有效期是否为数值或有效
                // isNaN是js的内置函数，用于判断一个值是否为NaN（非数值），
                // 非数值返回true，数值返回false
                // 还可以限制一下有效期为整数，这里就不做了
                if (isNaN(expire) || expire < 1) {
                    throw new Error('有效期应为一个有效数值')
                }
                // 86_400_000一天时间的毫秒数，_是数值分隔符
                let time = expire
                let obj = {
                    data: value, //存储值
                    time: Date.now(), //存值时间戳
                    expire: time, //过期时间
                }
                // 注意，localStorage不能直接存储对象类型，sessionStorage也一样
                // 需要先用JSON.stringify()将其转换成字符串，取值时再通过JSON.parse()转换回来
                localStorage.setItem(key, JSON.stringify(obj))
            }

            // 取值函数
            // 接收一个参数，存值的键
            Storage.prototype.getCanExpireLocal = key => {
                let val = localStorage.getItem(key)
                // 如果没有值就直接返回null
                if (!val) return val
                // 存的时候转换成了字符串，现在转回来
                val = JSON.parse(val)
                // 存值时间戳 +  有效时间 = 过期时间戳
                // 如果当前时间戳大于过期时间戳说明过期了，删除值并返回提示
                if (Date.now() > val.time + val.expire) {
                    localStorage.removeItem(key)
                    return undefined
                }
                return val.data
            }
        }
    }
})

</script>

<style lang="scss">

</style>
