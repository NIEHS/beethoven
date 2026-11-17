#!/usr/bin/env bash
# Install software only: do not start Spark or execute a test application.
set -euo pipefail
work=/opt/beethoven-build/spark-downloads
mkdir -p "$work" /opt/spark /opt/java21 /opt/xgboost
cd "$work"

curl -fL --retry 3 \
    https://archive.apache.org/dist/spark/spark-4.2.0/spark-4.2.0-bin-hadoop3.tgz \
    -o spark.tgz
echo '3a6559e8546ff387db8fe7a04b8fe4008853b467b972c2e343df8e14a81450534777719fc5d4998dd96519a86fdf9de140cee753c61e2e94db044d5f9555ddc4  spark.tgz' | sha512sum -c -
tar -xzf spark.tgz --strip-components=1 -C /opt/spark

# Match /ddn/gs1/tools/java/java21 used by the current cluster's spark-env.sh.
curl -fL --retry 3 \
    https://download.oracle.com/java/21/archive/jdk-21.0.1_linux-x64_bin.tar.gz \
    -o java21.tgz
echo '7c1f7689db0f4b48ee6978029c4a1aecd1442a8a7637cdf43a5471d0c79712a8  java21.tgz' | sha256sum -c -
tar -xzf java21.tgz --strip-components=1 -C /opt/java21

# XGBoost 3.4.0's published Spark JAR already includes the native/core library.
# Include its runtime Maven dependencies so R/spark.R can use XGBOOST_JARS
# without resolving Maven packages during a pipeline job.
for artifact in \
    ml/dmlc/xgboost4j-spark_2.13/3.4.0/xgboost4j-spark_2.13-3.4.0.jar \
    com/esotericsoftware/kryo/5.6.2/kryo-5.6.2.jar \
    com/esotericsoftware/reflectasm/1.11.9/reflectasm-1.11.9.jar \
    org/objenesis/objenesis/3.4/objenesis-3.4.jar \
    com/esotericsoftware/minlog/1.3.1/minlog-1.3.1.jar \
    commons-logging/commons-logging/1.3.4/commons-logging-1.3.4.jar
do
    filename=${artifact##*/}
    url="https://repo.maven.apache.org/maven2/$artifact"
    curl -fsSL --retry 3 "$url" -o "/opt/xgboost/$filename"
    curl -fsSL --retry 3 "$url.sha1" -o "$filename.sha1"
    read -r digest < "$filename.sha1" || true
    [[ "$digest" =~ ^[[:xdigit:]]{40}$ ]]
    printf '%s  %s\n' "$digest" "/opt/xgboost/$filename" | sha1sum -c -
done
sha256sum /opt/xgboost/*.jar > /opt/beethoven-build/xgboost-jars.sha256
cp /opt/spark/RELEASE /opt/beethoven-build/spark-release.txt
cp /opt/java21/release /opt/beethoven-build/java-release.txt
rm -rf "$work"
